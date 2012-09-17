/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editpolicies;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.gef.Request;

public class InsertTransformedMetamodelElementRequest extends Request {

	private EObject metamodelElement = null;

	public EObject getMetamodelElement() {
		return metamodelElement;
	}

	public void setMetamodelElement(EObject metamodelElement) {
		this.metamodelElement = metamodelElement;
	}
}
