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

package org.eclipse.wazaabi.engine.edp.converters.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.edp.converters.BundledConverter;
import org.eclipse.wazaabi.engine.edp.converters.BundledConverterFactory;
import org.eclipse.wazaabi.engine.edp.converters.ComposedBundledConverterFactory;

public class ComposedBundledConverterFactoryImpl implements
		ComposedBundledConverterFactory {

	private List<BundledConverterFactory> BundledConverterFactories = new ArrayList<BundledConverterFactory>();

	public void addBundledConverterFactory(BundledConverterFactory factory) {
		if (factory == null || factory.getFactoryID() == null
				|| "".equals(factory.getFactoryID())) //$NON-NLS-1$
			return;
		for (BundledConverterFactory item : BundledConverterFactories)
			if (item.getFactoryID().equals(factory.getFactoryID()))
				return;
//		System.out.println(":::: adding " + factory);
		BundledConverterFactories.add(factory);
	}

	public void removeBundledConverterFactory(BundledConverterFactory factory) {
//		System.out.println(":::: removing " + factory);
		BundledConverterFactories.remove(factory);
	}

	public BundledConverter createBundledConverter(Object context, String id) {
		if (id == null)
			return null;
		final BundledConverterFactory factory = getFactoryFor(context, id);
		if (factory != null)
			return factory.createBundledConverter(context, id);
		return null;
	}

	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof String) {
			final BundledConverterFactory factory = getFactoryFor(context,
					(String) source);
			if (factory != null)
				return factory.isFactoryFor(context, source);
		}
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	protected BundledConverterFactory getFactoryFor(Object context, String id) {
		if (id == null)
			return null;
		for (BundledConverterFactory factory : BundledConverterFactories)
			if (factory.isFactoryFor(context, id))
				return factory;
		return null;
	}


}
