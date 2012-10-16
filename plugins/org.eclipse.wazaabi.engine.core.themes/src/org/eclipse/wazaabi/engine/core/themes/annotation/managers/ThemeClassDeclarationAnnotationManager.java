/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.core.themes.annotation.managers;

import java.util.HashMap;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.themes.Themes.Theme;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public class ThemeClassDeclarationAnnotationManager31 extends AnnotationManager {

	public static final String CORE_THEMES_CLASS_ANNOTATION_SOURCE = "http://www.wazaabi.org/core/themes/class"; //$NON-NLS-1$
	// TODO : move this
	public static final String CORE_THEMES_PARAMETER_ANNOTATION_SOURCE = "http://www.wazaabi.org/core/themes/parameter"; //$NON-NLS-1$
	protected static final String CLASS_KEY = "class"; //$NON-NLS-1$

	public ThemeClassDeclarationAnnotationManager31(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {
		if (getAnnotation() == null)
			return;

		// We iterate over the container's Annotations in order to check if we
		// need to process any existing theme declaration
		if (getAnnotation().eContainer() instanceof AnnotatedElement) {
			for (Annotation otherAnnotation : ((AnnotatedElement) getAnnotation()
					.eContainer()).getAnnotations()) {
				// did we find another class declaration annotation before ?
				if (otherAnnotation != getAnnotation()
						&& isThemeClassAnnotation(otherAnnotation))
					break;
				// does it exist any themes declaration in this
				// AnnotatedElement?
				if (ThemeDeclarationAnnotationManager.CORE_THEMES_ANNOTATION_SOURCE
						.equals(otherAnnotation.getSource()))
					new ThemeDeclarationAnnotationManager(otherAnnotation)
							.forceProcessAnnotation(host);
			}
		}

		for (AnnotationContent content : getAnnotation().getContents()) {
			if (CLASS_KEY.equals(content.getKey()))
				processClassDeclaration(host, content.getValue());
		}
	}

	static protected boolean isThemeClassAnnotation(Annotation a) {
		if (CORE_THEMES_CLASS_ANNOTATION_SOURCE.equals(a.getSource())) {
			for (AnnotationContent content : a.getContents())
				if (CLASS_KEY.equals(content.getKey()))
					return true;
		}
		return false;
	}

	protected void processMergeFirst(AbstractWidgetEditPart host, Theme theme) {
	}

	protected boolean checkSourceCorrectness(String source) {
		return CORE_THEMES_CLASS_ANNOTATION_SOURCE.equals(source);
	}

	public static String getCoreThemeClassDeclaration(Widget widget) {
		for (Annotation annotation : widget.getAnnotations())
			if (CORE_THEMES_CLASS_ANNOTATION_SOURCE.equals(annotation
					.getSource()))
				for (AnnotationContent annotationContent : annotation
						.getContents())
					if (CLASS_KEY.equals(annotationContent.getKey()))
						return annotationContent.getValue();
		return null;
	}

	protected void processClassDeclaration(AbstractWidgetEditPart host,
			String className) {

		if (className == null || "".equals(className)) //$NON-NLS-1$
			return;
		if (!(host.getModel() instanceof Widget))
			return;
		Widget target = (Widget) host.getModel();
		HashMap<String, Object> variables = getVariables(target);

		Theme appenedTheme = ThemeDeclarationAnnotationManager
				.resolveWidgetInTheme_old(target, className);
		if (appenedTheme != null)
			for (Widget source : appenedTheme.getChildren())
				applyAppend(source, target, variables);
	}

	protected void applyAppend(Widget themedWidget, Widget uiWidget,
			HashMap<String, Object> variables) {
		// First we process EventHandlers
		for (EventHandler eventHandler : themedWidget.getHandlers()) {
			EventHandler clone = (EventHandler) EcoreUtil.copy(eventHandler);
			replaceVariables(clone, uiWidget, variables);
			uiWidget.getHandlers().add(clone);
		}

		// styleRules
		for (StyleRule rule : themedWidget.getStyleRules()) {
			StyleRule uiRule = null;
			for (StyleRule _uiRule : uiWidget.getStyleRules()) {
				if (rule.getPropertyName().equals(_uiRule.getPropertyName())
						&& rule.getClass().equals(_uiRule.getClass())) {
					uiRule = _uiRule;
					break;
				}
			}
			if (uiRule == null) {
				StyleRule newRule = (StyleRule) EcoreUtil.copy(rule);
				uiWidget.getStyleRules().add(0, newRule);
				break;
			}

			// TODO : some tests are recurrent
			for (EStructuralFeature feature : rule.eClass()
					.getEAllStructuralFeatures()) {
				boolean isSetWithDefaultValue = true;
				if (feature != CoreStylesPackage.Literals.STYLE_RULE__PROPERTY_NAME
						&& !feature.isMany()
						&& !feature.isTransient()
						&& feature.isChangeable() && !feature.isVolatile()) {
					if (feature.getDefaultValue() != null)
						isSetWithDefaultValue = feature.getDefaultValue()
								.equals(rule.eGet(feature));
					else
						isSetWithDefaultValue = (rule.eGet(feature) == null);
					if (!isSetWithDefaultValue
							&& ((feature.getDefaultValue() != null && feature
									.getDefaultValue().equals(
											uiRule.eGet(feature))) || feature
									.getDefaultValue() == null
									&& uiRule.eGet(feature) == null)) {
						if ((rule.eGet(feature) != null && !rule.eGet(feature)
								.equals(uiWidget.eGet(feature)))
								|| (rule.eGet(feature) == null && uiWidget
										.eGet(feature) != null)) {
							uiRule.eSet(feature, rule.eGet(feature));
						}
					}
				}
			}
		}

	}

	protected void replaceVariables(EventHandler eventHandler,
			Widget destination, HashMap<String, Object> variables) {
		for (Parameter parameter : eventHandler.getParameters()) {
			if (parameter instanceof StringParameter) {
				final String value = ((StringParameter) parameter).getValue();
				if (value != null && !"".equals(value)
						&& value.startsWith("${") && value.endsWith("}")
						&& value.length() > 2) {
					String variableName = value
							.substring(2, value.length() - 1);
					((StringParameter) parameter).setValue((String) variables
							.get(variableName));
				}
			}
		}
	}

	protected HashMap<String, Object> getVariables(Widget widget) {
		HashMap<String, Object> variables = new HashMap<String, Object>();
		for (Annotation annotation : widget.getAnnotations()) {
			if (CORE_THEMES_PARAMETER_ANNOTATION_SOURCE.equals(annotation
					.getSource())) {
				for (AnnotationContent content : annotation.getContents()) {
					if (content.getKey() != null
							&& !"".equals(content.getKey())) {
						variables.put(content.getKey(), content.getValue());
					}
				}
			}
		}
		return variables;
	}

}
