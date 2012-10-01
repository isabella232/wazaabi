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
import org.eclipse.wazaabi.engine.core.themes.annotation.managers.CoreThemeAnnotationManager;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;

public class CoreThemesAnnotationManagerFactory implements
		AnnotationManagerFactory {

	public AnnotationManager createAnnotationManager(Annotation annotation) {
		if (annotation != null) {
			final String source = annotation.getSource();
			if (source != null && !"".equals(source)) { //$NON-NLS-1$
				if (CoreThemeAnnotationManager.CORE_THEMES_ANNOTATION_SOURCE
						.equals(source))
					return new CoreThemeAnnotationManager(annotation);
			}
		}
		return null;
	}

	public boolean isFactoryFor(Annotation annotation) {
		if (annotation != null) {
			final String source = annotation.getSource();
			if (source != null && !"".equals(source)) { //$NON-NLS-1$
				return CoreThemeAnnotationManager.CORE_THEMES_ANNOTATION_SOURCE
						.equals(source);
			}
		}
		return false;
	}


}
