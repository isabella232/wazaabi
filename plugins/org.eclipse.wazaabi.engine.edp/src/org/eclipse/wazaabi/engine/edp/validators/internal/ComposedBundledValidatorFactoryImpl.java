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

package org.eclipse.wazaabi.engine.edp.validators.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidatorFactory;
import org.eclipse.wazaabi.engine.edp.validators.ComposedBundledValidatorFactory;

public class ComposedBundledValidatorFactoryImpl implements
		ComposedBundledValidatorFactory {

	private List<BundledValidatorFactory> BundledValidatorFactories = new ArrayList<BundledValidatorFactory>();

	public void addBundledValidatorFactory(BundledValidatorFactory factory) {
		if (factory == null || factory.getFactoryID() == null
				|| "".equals(factory.getFactoryID())) //$NON-NLS-1$
			return;
		for (BundledValidatorFactory item : BundledValidatorFactories)
			if (item.getFactoryID().equals(factory.getFactoryID()))
				return;
		BundledValidatorFactories.add(factory);
	}

	public void removeBundledValidatorFactory(BundledValidatorFactory factory) {
//		System.out.println(":::: removing " + factory);
		BundledValidatorFactories.remove(factory);
	}

	public BundledValidator createBundledValidator(Object context, String id) {
		if (id == null)
			return null;
		final BundledValidatorFactory factory = getFactoryFor(context, id);
		if (factory != null)
			return factory.createBundledValidator(context, id);
		return null;
	}

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof String) {
			final BundledValidatorFactory factory = getFactoryFor(context,
					(String) source);
			if (factory != null)
				return factory.isFactoryFor(context, source);
		}
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	protected BundledValidatorFactory getFactoryFor(Object context, String id) {
		if (id == null)
			return null;
		for (BundledValidatorFactory factory : BundledValidatorFactories)
			if (factory.isFactoryFor(context, id))
				return factory;
		return null;
	}


}
