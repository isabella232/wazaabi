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
import java.io.UnsupportedEncodingException;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.themes.Themes.Theme;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public class ThemeClasseDeclarationAnnotationManager extends AnnotationManager {

	public static final String CORE_THEMES_CLASS_ANNOTATION_SOURCE = "http://www.wazaabi.org/core/themes/class"; //$NON-NLS-1$
	protected static final String CLASS_KEY = "class"; //$NON-NLS-1$

	public ThemeClasseDeclarationAnnotationManager(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {
		if (getAnnotation() == null)
			return;
		for (AnnotationContent content : getAnnotation().getContents()) {
		}
	}

	protected void processMergeFirst(AbstractWidgetEditPart host, Theme theme) {
	}

	protected boolean checkSourceCorrectness(String source) {
		return CORE_THEMES_CLASS_ANNOTATION_SOURCE.equals(source);
	}

}
