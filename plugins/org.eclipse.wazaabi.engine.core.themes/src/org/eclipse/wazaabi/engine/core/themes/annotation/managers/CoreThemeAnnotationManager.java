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

public class CoreThemeAnnotationManager extends AnnotationManager {

	public static final String CORE_THEMES_ANNOTATION_SOURCE = "http://www.wazaabi.org/core-themes"; //$NON-NLS-1$
	protected static final String INSERT_INLINE_KEY = "insert-inline"; //$NON-NLS-1$
	protected static final String INSERT_URI_KEY = "insert-uri"; //$NON-NLS-1$
	protected static final String MERGE_FIRST_INLINE_KEY = "merge-first-inline"; //$NON-NLS-1$
	protected static final String MERGE_FIRST_URI_KEY = "merge-first-uri"; //$NON-NLS-1$

	protected static final String INLINE_THEME_KEY = CORE_THEMES_ANNOTATION_SOURCE
			+ "/inline"; //$NON-NLS-1$
	protected static final String MERGE_FIRST_THEME_KEY = CORE_THEMES_ANNOTATION_SOURCE
			+ "/merge-first"; //$NON-NLS-1$

	public CoreThemeAnnotationManager(Annotation annotation) {
		super(annotation);
	}

	public void processAnnotation(AbstractWidgetEditPart host) {
		if (getAnnotation() == null)
			return;
		for (AnnotationContent content : getAnnotation().getContents()) {
			if (INSERT_INLINE_KEY.equals(content.getKey()))
				processInsert(host, parseInline(content.getValue()));
			else if (INSERT_URI_KEY.equals(content.getKey()))
				processInsert(host, parseURI(host, content.getValue()));
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
		return null;
	}

	protected void processInsert(AbstractWidgetEditPart host, Theme theme) {
		if (theme == null)
			return;
	}

	protected void processMergeFirst(AbstractWidgetEditPart host, Theme theme) {
	}

	protected boolean checkSourceCorrectness(String source) {
		return CORE_THEMES_ANNOTATION_SOURCE.equals(source);
	}

}
