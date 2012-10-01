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

package org.eclipse.wazaabi.engine.core.annotations.factories;

import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.engine.core.annotations.managers.SetFeatureAnnotationManager;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;

public class CoreAnnotationManagerFactory implements AnnotationManagerFactory {

	@Override
	public AnnotationManager createAnnotationManager(Annotation annotation) {
		if (annotation != null) {
			final String source = annotation.getSource();
			if (source != null && !"".equals(source)) { //$NON-NLS-1$
				if (SetFeatureAnnotationManager.SET_FEATURE_ANNOTATION_SOURCE
						.equals(source))
					return new SetFeatureAnnotationManager(annotation);
			}
		}
		return null;
	}

	@Override
	public boolean isFactoryFor(Annotation annotation) {
		if (annotation != null) {
			final String source = annotation.getSource();
			if (source != null && !"".equals(source)) { //$NON-NLS-1$
				return SetFeatureAnnotationManager.SET_FEATURE_ANNOTATION_SOURCE
						.equals(source);
			}
		}
		return false;
	}
}
