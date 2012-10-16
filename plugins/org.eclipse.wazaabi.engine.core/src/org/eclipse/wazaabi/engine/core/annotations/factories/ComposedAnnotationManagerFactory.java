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

import java.util.List;

import org.eclipse.wazaabi.engine.core.annotations.managers.AnnotationManager;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public interface ComposedAnnotationManagerFactory {

	public void addAnnotationManagerFactory(AnnotationManagerFactory factory);

	public void removeAnnotationManagerFactory(AnnotationManagerFactory factory);

	public List<AnnotationManager> getRelevantAnnotationManagers(Widget widget);

}
