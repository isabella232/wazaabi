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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

public class EventDescriptorFactory extends AbstractDescriptorFactory {
	private static final EventDescriptor[] descriptors = new EventDescriptor[] {
			new EventDescriptor("core:ui:selection", "selection",
					"core:ui:selection", EDPEventsPackage.eNS_URI,
					EDPEventsPackage.Literals.EVENT.getName()),
			new EventDescriptor("core:ui:refresh", "refresh",
					"core:ui:refresh", EDPEventsPackage.eNS_URI,
					EDPEventsPackage.Literals.EVENT.getName()) };

	@Override
	public Set<AbstractDescriptor> getDescriptors(EClass eClass) {
		if (EDPHandlersPackage.Literals.EVENT_HANDLER.isSuperTypeOf(eClass)) {
			Set<AbstractDescriptor> result = new HashSet<AbstractDescriptor>();
			result.addAll(Arrays.asList(descriptors));
			return result;
		}
		return Collections.emptySet();
	}

	@Override
	public AbstractDescriptor getDescriptor(EObject eObject) {
		if (eObject instanceof Event)
			for (EventDescriptor eventDescriptor : descriptors)
				if (eventDescriptor.getId().equals(((Event) eObject).getId()))
					return eventDescriptor;
		return null;
	}
}
