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
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.themes.Themes.CoreThemesFactory;
import org.eclipse.wazaabi.mm.core.themes.Themes.Theme;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public class ThemeDeclarationAnnotationManager extends AnnotationManager {

	public static final String CORE_THEMES_DECLARATION_ANNOTATION_SOURCE = "http://www.wazaabi.org/core/themes/declaration"; //$NON-NLS-1$
	protected static final String APPEND_INLINE_KEY = "append-inline"; //$NON-NLS-1$
	protected static final String APPEND_URI_KEY = "append-uri"; //$NON-NLS-1$
	protected static final String MERGE_FIRST_INLINE_KEY = "merge-first-inline"; //$NON-NLS-1$
	protected static final String MERGE_FIRST_URI_KEY = "merge-first-uri"; //$NON-NLS-1$

	protected static final String APPEND_CLASSES_THEME_KEY = CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
			+ "/append-classes-theme"; //$NON-NLS-1$
	protected static final String MERGE_FIRST_CLASSES_THEME_KEY = CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
			+ "/merge-first-classes-theme"; //$NON-NLS-1$
	protected static final String APPEND_WIDGET_THEME_KEY = CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
			+ "/append-widget-theme"; //$NON-NLS-1$
	protected static final String MERGE_FIRST_WIDGET_THEME_KEY = CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
			+ "/merge-first-widget-theme"; //$NON-NLS-1$

	public ThemeDeclarationAnnotationManager(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {
		if (getAnnotation() == null)
			return;
		for (AnnotationContent content : getAnnotation().getContents()) {
			if (APPEND_INLINE_KEY.equals(content.getKey()))
				processAppend(host, parseInline(content.getValue()));
			else if (APPEND_URI_KEY.equals(content.getKey()))
				processAppend(host, parseURI(host, content.getValue()));
			else if (MERGE_FIRST_INLINE_KEY.equals(content.getKey()))
				processMergeFirst(host, parseInline(content.getValue()));
			else if (MERGE_FIRST_URI_KEY.equals(content.getKey()))
				processMergeFirst(host, parseURI(host, content.getValue()));
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
				addClass(host, APPEND_CLASSES_THEME_KEY, classValue, widget);
			else
				addWidget(host, APPEND_WIDGET_THEME_KEY, widget);
		}
	}

	protected void processMergeFirst(AbstractWidgetEditPart host, Theme theme) {
	}

	protected boolean checkSourceCorrectness(String source) {
		return CORE_THEMES_DECLARATION_ANNOTATION_SOURCE.equals(source);
	}

	@SuppressWarnings("unchecked")
	protected void addClass(AbstractWidgetEditPart host, String contextKey,
			String classValue, Widget widget) {
		Widget target = (Widget) host.getModel();
		Hashtable<String, Widget> themeClasses = null;
		if (target.get(contextKey) instanceof Hashtable<?, ?>)
			themeClasses = (Hashtable<String, Widget>) target.get(contextKey);
		else {
			themeClasses = new Hashtable<String, Widget>();
			target.set(contextKey, themeClasses);
		}
		themeClasses.put(classValue, widget);
	}

	@SuppressWarnings("unchecked")
	protected void addWidget(AbstractWidgetEditPart host, String contextKey,
			Widget widget) {
		Widget target = (Widget) host.getModel();
		List<Widget> themeWidgets = null;
		if (target.get(contextKey) instanceof List<?>)
			themeWidgets = (List<Widget>) target.get(contextKey);
		else {
			themeWidgets = new ArrayList<Widget>();
			target.set(contextKey, themeWidgets);
		}
		themeWidgets.add(widget);
	}

	public static Theme resolveFirstMergedTheme(Widget widget, String className) {
		return null;
	}

	public static Theme resolveAppenedTheme(Widget widget, String className) {
		Widget current = widget;

		Object value = current.get(APPEND_CLASSES_THEME_KEY);
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
