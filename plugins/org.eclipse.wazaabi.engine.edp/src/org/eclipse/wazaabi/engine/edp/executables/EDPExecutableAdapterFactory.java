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

package org.eclipse.wazaabi.engine.edp.executables;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.adapters.ActionAdapterImpl;
import org.eclipse.wazaabi.engine.edp.adapters.ConditionAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.ConverterAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventDispatcherAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.SequenceAdapterImpl;
import org.eclipse.wazaabi.engine.edp.adapters.ValidatorAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractDeferredAdapter;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;

public class EDPExecutableAdapterFactory implements ExecutableAdapterFactory {

	public static final String FACTORY_ID = EDPExecutableAdapterFactory.class
			.getName();

	public boolean isFactoryFor(Object callingContext, Object model,
			Object creationHint) {
		if (model instanceof EObject) {
			if (((EObject) model).eClass().getEPackage() == EDPHandlersPackage.eINSTANCE) {
				// we assume that a sequence is always created by an
				// EventHandler
				if (((((EObject) model).eClass()
						.equals(EDPHandlersPackage.Literals.SEQUENCE))))
					return callingContext instanceof EventHandlerAdapter;
				else
					return model instanceof Executable;
			}
		}
		return false;
	}

	public String getFactoryID() {
		return FACTORY_ID;
	}

	@Override
	public Adapter createAdapter(final Object callingContext, EObject model,
			Object creationHint) {

		if (model == null)
			return null;

		Adapter adapter = null;
		// Since our Executable is created from within a EventHandler
		if (model.eClass().equals(EDPHandlersPackage.Literals.SEQUENCE)
				&& callingContext instanceof EventHandlerAdapter)
			return new SequenceAdapterImpl() {

				@Override
				protected EventDispatcherAdapter getEventDispatcherAdapter() {
					return ((EventHandlerAdapter) callingContext)
							.getEventDispatcherAdapter();
				}

			};

		if (model.eClass() == EDPHandlersPackage.Literals.CONVERTER)
			adapter = new ConverterAdapter();
		else if (model.eClass() == EDPHandlersPackage.Literals.ACTION)
			adapter = new ActionAdapterImpl();
		else if (model.eClass() == EDPHandlersPackage.Literals.CONDITION)
			adapter = new ConditionAdapter();
		else if (model.eClass() == EDPHandlersPackage.Literals.VALIDATOR)
			adapter = new ValidatorAdapter();

		if (creationHint instanceof Registry
				&& adapter instanceof AbstractDeferredAdapter)
			((AbstractDeferredAdapter) adapter)
					.setRegistry((Registry) creationHint);
		return adapter;
	}
}
