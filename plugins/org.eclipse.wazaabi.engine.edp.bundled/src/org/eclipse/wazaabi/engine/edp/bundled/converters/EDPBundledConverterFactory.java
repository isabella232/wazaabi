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

import org.eclipse.wazaabi.engine.edp.converters.BundledConverter;
import org.eclipse.wazaabi.engine.edp.converters.BundledConverterFactory;



public class EDPBundledConverterFactory implements BundledConverterFactory {


	public boolean isFactoryFor(Object context, Object source) {
		if (source instanceof String)
			return true;
		return false;
	}

	public String getFactoryID() {
		return getClass().getName();
	}

	
	public BundledConverter createBundledConverter(Object context, String id) {
		if ("bundledHelloStringConverter".equals(id))
			return new BundledHelloStringConverter();
		else if ("bundledIntToStringConverter".equals(id))
			return new BundledIntToStringConverter();
		else if ("bundledStringToIntConverter".equals(id))
			return new BundledStringToIntConverter();
		else if ("TestBasicConverter".equals(id))
			return new TestBundledBasicConverter();
		
		return null;
	}

}
