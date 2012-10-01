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

import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.themes.Themes.Theme;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public class ThemeClassDeclarationAnnotationManager extends AnnotationManager {

	public static final String CORE_THEMES_CLASS_ANNOTATION_SOURCE = "http://www.wazaabi.org/core/themes/class"; //$NON-NLS-1$
	protected static final String CLASS_KEY = "class"; //$NON-NLS-1$

	public ThemeClassDeclarationAnnotationManager(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {
		if (getAnnotation() == null)
			return;
		for (AnnotationContent content : getAnnotation().getContents()) {
			if (CLASS_KEY.equals(content.getKey()))
				processClassDeclaration(host, content.getValue());
		}
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
getInsertedThemes ();
getFirstMergedThemes();
	}

}
