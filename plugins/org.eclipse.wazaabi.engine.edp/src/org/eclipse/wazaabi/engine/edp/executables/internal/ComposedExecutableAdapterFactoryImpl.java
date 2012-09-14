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

package org.eclipse.wazaabi.engine.edp.executables.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;
import org.eclipse.wazaabi.engine.edp.executables.ComposedExecutableAdapterFactory;
import org.eclipse.wazaabi.engine.edp.executables.ExecutableAdapterFactory;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;



public class ComposedExecutableAdapterFactoryImpl implements
		ComposedExecutableAdapterFactory {

	private List<ExecutableAdapterFactory> executableAdapterFactories = new ArrayList<ExecutableAdapterFactory>();

	public void addExecutableAdapterFactory(ExecutableAdapterFactory factory) {
		if (factory == null || factory.getFactoryID() == null
				|| "".equals(factory.getFactoryID())) //$NON-NLS-1$
			return;
		for (ExecutableAdapterFactory item : executableAdapterFactories)
			if (item.getFactoryID().equals(factory.getFactoryID()))
				return;
//		System.out.println(":::: adding " + factory);
		executableAdapterFactories.add(factory);
	}

	public void removeExecutableAdapterFactory(ExecutableAdapterFactory factory) {
//		System.out.println(":::: removing " + factory);
		executableAdapterFactories.remove(factory);
	}

	public ExecutableAdapter createExecutableAdapter(Object context, Executable executable) {
		if (executable == null)
			return null;
		final ExecutableAdapterFactory factory = getFactoryFor(context, executable);
		if (factory != null)
			return factory.createExecutableAdapter(context, executable);
		return null;
	}

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof Executable) {
			final ExecutableAdapterFactory factory = getFactoryFor(context,
					(Executable) source);
			if (factory != null)
				return factory.isFactoryFor(context, source);
		}
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	protected ExecutableAdapterFactory getFactoryFor(Object context, Executable executable) {
		if (executable == null)
			return null;
		for (ExecutableAdapterFactory factory : executableAdapterFactories)
			if (factory.isFactoryFor(context, executable))
				return factory;
		return null;
	}


}
