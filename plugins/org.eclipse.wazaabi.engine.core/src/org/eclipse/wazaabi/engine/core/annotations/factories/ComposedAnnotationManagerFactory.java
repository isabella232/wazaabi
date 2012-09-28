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

public interface ComposedAnnotationManagerFactory extends
		AnnotationManagerFactory {

	public void addAnnotationManagerFactory(AnnotationManagerFactory factory);

	public void removeAnnotationManagerFactory(AnnotationManagerFactory factory);

}
