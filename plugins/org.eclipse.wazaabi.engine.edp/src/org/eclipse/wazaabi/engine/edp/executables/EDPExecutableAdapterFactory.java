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
import org.eclipse.wazaabi.engine.edp.adapters.ValidatorAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractDeferredAdapter;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;

public class EDPExecutableAdapterFactory implements ExecutableAdapterFactory {

	public static final String FACTORY_ID = EDPExecutableAdapterFactory.class
			.getName();

	public boolean isFactoryFor(Object callingContext, Object model) {
		if (model instanceof Executable)
			return true;
		return false;
	}

	public String getFactoryID() {
		return FACTORY_ID;
	}

	@Override
	public Adapter createAdapter(Object callingContext, EObject model,
			Object creationHint) {

		if (model == null)
			return null;

		Adapter adapter = null;
		// if (executable.eClass() == EDPHandlersPackage.Literals.SEQUENCE)
		// return new SequenceAdapterImpl();
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
