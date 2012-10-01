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

public class AnnotationManager {

	private AbstractWidgetEditPart host = null;
	private Annotation annotation = null;

	protected Annotation getAnnotation() {
		return annotation;
	}

	protected void setAnnotation(Annotation target) {
		this.annotation = target;
	}

	protected AbstractWidgetEditPart getHost() {
		return host;
	}

	protected void setHost(AbstractWidgetEditPart host) {
		this.host = host;
	}

}
