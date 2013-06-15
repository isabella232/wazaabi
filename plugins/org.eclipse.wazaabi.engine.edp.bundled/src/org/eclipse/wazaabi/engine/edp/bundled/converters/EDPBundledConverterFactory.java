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

package org.eclipse.wazaabi.engine.edp.bundled.converters;

import org.eclipse.wazaabi.engine.edp.converters.BundledConverterFactory;

public class EDPBundledConverterFactory implements BundledConverterFactory {

	public static final String FACTORY_ID = EDPBundledConverterFactory.class
			.getName();

	public boolean isFactoryFor(Object callingContext, Object model, Object creationHint) {
		if (model instanceof String)
			return true;
		return false;
	}

	public String getFactoryID() {
		return FACTORY_ID;
	}

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		if ("bundledHelloStringConverter".equals(model))
			return new BundledHelloStringConverter();
		else if ("bundledIntToStringConverter".equals(model))
			return new BundledIntToStringConverter();
		else if ("bundledStringToIntConverter".equals(model))
			return new BundledStringToIntConverter();
		else if ("TestBasicConverter".equals(model))
			return new TestBundledBasicConverter();
		return null;
	}

}
