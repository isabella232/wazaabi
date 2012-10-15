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
import java.util.Hashtable;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.themes.Themes.CoreThemesFactory;
import org.eclipse.wazaabi.mm.core.themes.Themes.Theme;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public class ThemeDeclarationAnnotationManager extends AnnotationManager {

	public static final String CORE_THEMES_DECLARATION_ANNOTATION_SOURCE = "http://www.wazaabi.org/core/themes/declaration"; //$NON-NLS-1$
	protected static final String INLINE_KEY = "inline"; //$NON-NLS-1$
	protected static final String URI_KEY = "uri"; //$NON-NLS-1$

	protected static final String CLASSES_DEFINITIONS_KEY = CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
			+ "/classes-definitions"; //$NON-NLS-1$
	protected static final String WIDGET_DEFINITIONS_KEY = CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
			+ "/widgets-definitions"; //$NON-NLS-1$

	public ThemeDeclarationAnnotationManager(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {
		if (getAnnotation() == null)
			return;

		// if any theme class declaration exists before this annotation, we
		// already processed this declaration

		if (getAnnotation().eContainer() instanceof AnnotatedElement) {
			for (Annotation otherAnnotation : ((AnnotatedElement) getAnnotation()
					.eContainer()).getAnnotations()) {
				// did we find another class declaration annotation before ?
				if (ThemeClassDeclarationAnnotationManager
						.isThemeClassAnnotation(otherAnnotation))
					return;
				// we iterate until we get our own annotation
				if (otherAnnotation == getAnnotation())
					break;
			}
		}
		forceProcessAnnotation(host);
	}

	protected void forceProcessAnnotation(AbstractWidgetEditPart host) {
		assert host != null;
		assert getAnnotation() != null;
		for (AnnotationContent content : getAnnotation().getContents()) {
			if (INLINE_KEY.equals(content.getKey()))
				processAppend(host, parseInline(content.getValue()));
			else if (URI_KEY.equals(content.getKey()))
				processAppend(host, parseURI(host, content.getValue()));
		}
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

	protected void processAppend(AbstractWidgetEditPart host, Theme theme) {
		if (theme == null)
			return;
		for (Widget widget : theme.getChildren()) {
			String classValue = ThemeClassDeclarationAnnotationManager
					.getCoreThemeClassDeclaration(widget);
			if (classValue != null && !"".equals(classValue)) //$NON-NLS-1$
				addClass(host, classValue, widget);
			else
				addWidget(host, widget);
		}
	}

	protected boolean checkSourceCorrectness(String source) {
		return CORE_THEMES_DECLARATION_ANNOTATION_SOURCE.equals(source);
	}

	/**
	 * This method adds a a set of theme rules to this host's model. This set of
	 * rules is associated to a class. This method ensures that the set of rules
	 * (hold by a Widget) is present only once in the host's model context.
	 * 
	 * @param host
	 *            The Editpart whose target is the model
	 * @param widget
	 *            The theme declaration
	 */
	@SuppressWarnings("unchecked")
	protected void addClass(AbstractWidgetEditPart host, String classValue,
			Widget widget) {
		Widget target = (Widget) host.getModel();
		Hashtable<String, Widget> themeClasses = null;
		if (target.get(CLASSES_DEFINITIONS_KEY) instanceof Hashtable<?, ?>)
			themeClasses = (Hashtable<String, Widget>) target
					.get(CLASSES_DEFINITIONS_KEY);
		else {
			themeClasses = new Hashtable<String, Widget>();
			target.set(CLASSES_DEFINITIONS_KEY, themeClasses);
		}
		themeClasses.put(classValue, widget);
	}

	/**
	 * This method adds a a set of theme rules to this host's model (without any
	 * class association). This method ensures that the set of rules (hold by a
	 * Widget) is present only once in the host's model context.
	 * 
	 * @param host
	 *            The Editpart whose target is the model
	 * @param widget
	 *            The theme declaration
	 */
	@SuppressWarnings("unchecked")
	protected void addWidget(AbstractWidgetEditPart host, Widget widget) {
		Widget target = (Widget) host.getModel();
		List<Widget> themeWidgets = null;
		if (target.get(WIDGET_DEFINITIONS_KEY) instanceof List<?>)
			themeWidgets = (List<Widget>) target.get(WIDGET_DEFINITIONS_KEY);
		else {
			themeWidgets = new ArrayList<Widget>();
			target.set(WIDGET_DEFINITIONS_KEY, themeWidgets);
		}
		themeWidgets.add(widget);
	}

	/**
	 * This method returns any theme definition found fir the given widget and
	 * className. It starts from the widget itself, tries to find a theme
	 * definition in the widget's context and iterate over its ancestors until
	 * something is found.
	 * 
	 * @param widget
	 * @param className
	 * @return
	 */

	// TODO : usecase "widget declaration exists without any class declaration"
	// not supported
	public static Theme resolveWidgetInTheme(Widget widget, String className) {
		Widget current = widget;

		Object value = current.get(CLASSES_DEFINITIONS_KEY);
		if (value instanceof Hashtable<?, ?>) {
			@SuppressWarnings("unchecked")
			Hashtable<String, Widget> hashtable = (Hashtable<String, Widget>) value;
			Widget w = hashtable.get(className);

			if (w != null && w.getClass().isAssignableFrom(widget.getClass())) {
				Theme theme = CoreThemesFactory.eINSTANCE.createTheme();
				theme.getChildren().add(w);
				return theme;
			}
		}
		return null;
	}
}
