/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors;

import org.eclipse.emf.ecore.EObject;

public class EventHandlerDescriptor extends AbstractDescriptor{

	public EventHandlerDescriptor(String id, String label, String description,
			String packageURI, String eClassName) {
		super(id, label, description, packageURI, eClassName);
	}

	@Override
	public EObject createNewInstance() {
		return null;
	}

}
