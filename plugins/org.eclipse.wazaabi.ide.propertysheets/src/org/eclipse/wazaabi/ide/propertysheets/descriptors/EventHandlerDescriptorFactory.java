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
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

public class EventHandlerDescriptorFactory extends AbstractDescriptorFactory {

	private static final EventHandlerDescriptor[] descriptors = new EventHandlerDescriptor[] {
			new EventHandlerDescriptor(
					EDPHandlersPackage.Literals.EVENT_HANDLER.getName(),
					EDPHandlersPackage.Literals.EVENT_HANDLER.getName(),
					EDPHandlersPackage.Literals.EVENT_HANDLER.getName(),
					EDPHandlersPackage.eNS_URI,
					EDPHandlersPackage.Literals.EVENT_HANDLER.getName()),
			new EventHandlerDescriptor(
					EDPHandlersPackage.Literals.BINDING.getName(),
					EDPHandlersPackage.Literals.BINDING.getName(),
					EDPHandlersPackage.Literals.BINDING.getName(),
					EDPHandlersPackage.eNS_URI,
					EDPHandlersPackage.Literals.BINDING.getName()) };

	@Override
	public Set<AbstractDescriptor> getDescriptors(EClass eClass) {
		if (EdpPackage.Literals.EVENT_DISPATCHER.isSuperTypeOf(eClass)) {
			Set<AbstractDescriptor> result = new HashSet<AbstractDescriptor>();
			result.addAll(Arrays.asList(descriptors));
			return result;
		}
		return Collections.emptySet();
	}

	@Override
	public AbstractDescriptor getDescriptor(EObject eObject) {
		throw new RuntimeException();
	}

}
