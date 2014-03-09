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

package org.eclipse.wazaabi.ide.propertysheets.descriptors;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.wazaabi.mm.edp.events.Event;

public class EventDescriptor extends AbstractDescriptor {

	public EventDescriptor(String id, String label, String description,
			String packageURI, String eClassName) {
		super(id, label, description, packageURI, eClassName);
	}

	@Override
	public EObject createNewInstance() {
		EFactory factory = EPackage.Registry.INSTANCE
				.getEFactory(getPackageURI());
		EPackage ePackage = EPackage.Registry.INSTANCE
				.getEPackage(getPackageURI());
		EClass eClass = null;
		if (ePackage != null)
			eClass = (EClass) ePackage.getEClassifier(getEClassName());
		if (factory != null && eClass != null) {
			Event event = (Event) factory.create(eClass);
			event.setId(getId());
			return event;
		}
		return null;
	}

}
