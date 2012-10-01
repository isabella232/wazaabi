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

package org.eclipse.wazaabi.engine.core.themes.annotation.factories;

import org.eclipse.wazaabi.engine.core.annotations.factories.AnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.themes.annotation.managers.ThemeClassDeclarationAnnotationManager;
import org.eclipse.wazaabi.engine.core.themes.annotation.managers.ThemeDeclarationAnnotationManager;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;

public class CoreThemesAnnotationManagerFactory implements
		AnnotationManagerFactory {

	public AnnotationManager createAnnotationManager(Annotation annotation) {
		if (annotation != null) {
			final String source = annotation.getSource();
			if (source != null && !"".equals(source)) { //$NON-NLS-1$
				if (ThemeDeclarationAnnotationManager.CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
						.equals(source))
					return new ThemeDeclarationAnnotationManager(annotation);
				if (ThemeClassDeclarationAnnotationManager.CORE_THEMES_CLASS_ANNOTATION_SOURCE
						.equals(source))
					return new ThemeClassDeclarationAnnotationManager(
							annotation);
			}
		}
		return null;
	}

	public boolean isFactoryFor(Annotation annotation) {
		if (annotation != null) {
			final String source = annotation.getSource();
			if (source != null && !"".equals(source)) { //$NON-NLS-1$
				return ThemeDeclarationAnnotationManager.CORE_THEMES_DECLARATION_ANNOTATION_SOURCE
						.equals(source)
						|| ThemeClassDeclarationAnnotationManager.CORE_THEMES_CLASS_ANNOTATION_SOURCE
								.equals(source);
			}
		}
		return false;
	}

}
