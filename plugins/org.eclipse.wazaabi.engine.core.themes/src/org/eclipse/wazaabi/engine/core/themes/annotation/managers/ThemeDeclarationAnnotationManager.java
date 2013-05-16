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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.themes.Themes.Theme;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PathEvent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public class ThemeDeclarationAnnotationManager extends AnnotationManager {

	public static final String CORE_THEMES_ANNOTATION_SOURCE = "http://www.wazaabi.org/core/themes"; //$NON-NLS-1$

	public static final String INLINE_KEY = "inline"; //$NON-NLS-1$
	public static final String URI_KEY = "uri"; //$NON-NLS-1$
	public static final String CLASS_KEY = "class"; //$NON-NLS-1$
	public static final String VARIABLE_KEY = "variable"; //$NON-NLS-1$

	private List<Widget> widgetDefinitions = new ArrayList<Widget>();
	private HashMap<String, Widget> classDefinitions = new HashMap<String, Widget>();

	public ThemeDeclarationAnnotationManager(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {
		if (host == null)
			return;
		Widget widget = (Widget) host.getModel();

		parseDeclarations(host);
		applyTheme(widget);

	}

	protected void parseDeclarations(AbstractWidgetEditPart host) {
		assert getAnnotation() != null;
		for (AnnotationContent content : getAnnotation().getContents()) {
			if (INLINE_KEY.equals(content.getKey()))
				parseTheme(parseInline(content.getValue()));
			else if (URI_KEY.equals(content.getKey()))
				parseTheme(parseURI(host, content.getValue()));
		}
	}

	protected void parseTheme(Theme theme) {
		if (theme == null)
			return;

		// TODO : we should merge class and widget definition instead of
		// overwriting each time
		for (Widget widget : theme.getChildren()) {
			String classValue = getCoreThemeClassDeclaration(widget);
			if (classValue != null && !"".equals(classValue)) //$NON-NLS-1$
				classDefinitions.put(classValue, widget);
			else
				widgetDefinitions.add(widget);
		}
	}

	public static String getCoreThemeClassDeclaration(Widget widget) {
		for (Annotation annotation : widget.getAnnotations())
			if (CORE_THEMES_ANNOTATION_SOURCE.equals(annotation.getSource()))
				for (AnnotationContent annotationContent : annotation
						.getContents())
					if (CLASS_KEY.equals(annotationContent.getKey()))
						return annotationContent.getValue();
		return null;
	}

	protected void applyTheme(Widget target) {
		HashMap<String, Object> variables = getVariables(target);
		for (Annotation annotation : target.getAnnotations())
			if (CORE_THEMES_ANNOTATION_SOURCE.equals(annotation.getSource()))
				for (AnnotationContent content : annotation.getContents())
					if (CLASS_KEY.equals(content.getKey()))
						processClassDeclaration(target, content.getValue(),
								variables);

		List<Widget> widgetsToApply = resolveWidgetsToApply(target);
		for (Widget w : widgetsToApply)
			applyThemeOnWidget(w, target, variables);

		if (target instanceof Container)
			for (Widget child : ((Container) target).getChildren())
				applyTheme(child);
	}

	protected Theme parseInline(String inlineValue) {
		Resource resource = new XMIResourceImpl();
		try {
			resource.load(
					new ByteArrayInputStream(inlineValue.getBytes("UTF-8")),
					null);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (resource.getContents().get(0) instanceof Theme)
			return (Theme) resource.getContents().get(0);
		return null;
	}

	protected Theme parseURI(AbstractWidgetEditPart host, String uri) {
		try {
			String baseURI = host.getViewer().getCodeLocatorBaseUri();
			if (baseURI != null && baseURI.length() != 0)
				uri = EDPSingletons.getComposedCodeLocator().getFullPath(
						baseURI, uri, host.getModel());

			InputStream in = EDPSingletons.getComposedCodeLocator()
					.getResourceInputStream(uri);
			if (in != null) {
				Resource resource = new XMIResourceImpl();
				try {
					resource.load(in, null);
				} catch (UnsupportedEncodingException e) {
					e.printStackTrace();
				}
				if (resource.getContents().get(0) instanceof Theme)
					return (Theme) resource.getContents().get(0);
				return null;
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	// TODO : use case 'BlankWidget' not supported at the moment
	protected Widget resolveWidgetToApply(Widget widget, String className) {
		Widget w = classDefinitions.get(className);
		if (w != null && w.getClass().isAssignableFrom(widget.getClass()))
			return w;
		return null;
	}

	// TODO : use case 'BlankWidget' not supported at the moment
	protected List<Widget> resolveWidgetsToApply(Widget widget) {
		List<Widget> result = new ArrayList<Widget>();
		for (Widget w : widgetDefinitions)
			if (w.getClass().isAssignableFrom(widget.getClass()))
				result.add(w);
		return result;
	}

	protected void processClassDeclaration(Widget target, String className,
			HashMap<String, Object> variables) {
		if (className == null || "".equals(className)) //$NON-NLS-1$
			return;

		Widget widgetToApply = resolveWidgetToApply(target, className);
		if (widgetToApply != null)
			applyThemeOnWidget(widgetToApply, target, variables);
	}

	protected void applyThemeOnWidget(Widget themedWidget, Widget uiWidget,
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
			} else {

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
							if ((rule.eGet(feature) != null && !rule.eGet(
									feature).equals(uiWidget.eGet(feature)))
									|| (rule.eGet(feature) == null && uiWidget
											.eGet(feature) != null)) {
								uiRule.eSet(feature, rule.eGet(feature));
							}
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
				String variableName = getVariableName(((StringParameter) parameter)
						.getValue());
				if (variableName != null && !"".equals(variableName)) //$NON-NLS-1$
					((StringParameter) parameter).setValue((String) variables
							.get(variableName));
			}
		}
		for (Event event : eventHandler.getEvents()) {
			if (event instanceof PathEvent) {
				String variableName = getVariableName(((PathEvent) event)
						.getPath());
				if (variableName != null && !"".equals(variableName)) //$NON-NLS-1$
					((PathEvent) event).setPath((String) variables
							.get(variableName));
			}
		}

	}

	protected String getVariableName(String str) {
		if (str != null && !"".equals(str) && str.startsWith("${")
				&& str.endsWith("}") && str.length() > 2)
			return str.substring(2, str.length() - 1);
		return null;
	}

	protected HashMap<String, Object> getVariables(Widget widget) {
		HashMap<String, Object> variables = new HashMap<String, Object>();
		for (Annotation annotation : widget.getAnnotations())
			if (CORE_THEMES_ANNOTATION_SOURCE.equals(annotation.getSource()))
				for (AnnotationContent content : annotation.getContents())
					if (VARIABLE_KEY.equals(content.getKey())) {
						String variableName = parseVariableName(content
								.getValue());
						if (variableName != null && !"".equals(variableName)) {
							Object value = parseVariable(variableName,
									content.getValue());
							if (value != null)
								variables.put(variableName,
										value != NULL_VALUE ? value : null);
						}
					}
		return variables;
	}

	protected String parseVariableName(String str) {
		if (str == null || "".equals(str)) //$NON-NLS-1$
			return null;
		int idx = str.indexOf('=');
		if (idx != -1)
			return str.substring(0, idx);
		return null;
	}

	private static final Object NULL_VALUE = new Object();

	protected Object parseVariable(String variableName, String str) {
		if (str.length() > variableName.length() + 1) {
			String value = str.substring(variableName.length() + 1);
			// is it null ?
			if ("null".equals(value))
				return NULL_VALUE;

			// is it a String variable ?
			if (value.length() > 2 && value.charAt(0) == '\''
					&& value.charAt(value.length() - 1) == '\'')
				return value.substring(1, value.length() - 1);

			// is it a Integer variable ?
			try {
				return new Integer(value);
			} catch (NumberFormatException e) {
				// NOTHING TO DO HERE
			}
		}
		return null;
	}
}
