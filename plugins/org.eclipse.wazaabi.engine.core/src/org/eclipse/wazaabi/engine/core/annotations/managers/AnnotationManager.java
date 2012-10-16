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

package org.eclipse.wazaabi.engine.core.annotations.managers;

import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.mm.core.annotations.Annotation;

public abstract class AnnotationManager {

	private final Annotation annotation;

	public AnnotationManager(Annotation annotation) {
		this.annotation = annotation;
	}

	protected Annotation getAnnotation() {
		return annotation;
	}

	public abstract void processAnnotation(AbstractWidgetEditPart host);

}
