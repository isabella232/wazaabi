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

import java.util.Set;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

public abstract class AbstractDescriptorFactory {

	public AbstractDescriptor findDescriptor(EObject eObject, String id) {
		for (AbstractDescriptor descriptor : getDescriptors(eObject.eClass()))
			if (descriptor.getId().equals(id))
				return descriptor;
		return null;
	}

	public abstract Set<AbstractDescriptor> getDescriptors(EClass eClass);

	public abstract AbstractDescriptor getDescriptor(EObject eObject);
}