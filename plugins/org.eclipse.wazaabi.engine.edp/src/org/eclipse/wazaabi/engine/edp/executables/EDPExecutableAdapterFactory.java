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
import org.eclipse.wazaabi.engine.edp.adapters.ActionAdapterImpl;
import org.eclipse.wazaabi.engine.edp.adapters.ConditionAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.ConverterAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.ValidatorAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;

public class EDPExecutableAdapterFactory implements ExecutableAdapterFactory {

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof Executable)
			return true;
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	public ExecutableAdapter createExecutableAdapter(Object context,
			Executable executable) {
		if (executable == null)
			return null;
//		if (executable.eClass() == EDPHandlersPackage.Literals.SEQUENCE)
//			return new SequenceAdapterImpl();
		else if (executable.eClass() == EDPHandlersPackage.Literals.CONVERTER)
			return new ConverterAdapter();
		else if (executable.eClass() == EDPHandlersPackage.Literals.ACTION)
			return new ActionAdapterImpl();
		else if (executable.eClass() == EDPHandlersPackage.Literals.CONDITION)
			return new ConditionAdapter();
		else if (executable.eClass() == EDPHandlersPackage.Literals.VALIDATOR)
			return new ValidatorAdapter();
		return null;
	}

	@Override
	public Adapter createAdapter(Object callingContext, EObject model) {
		// TODO Auto-generated method stub
		return null;
	}

}
