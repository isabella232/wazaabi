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

package org.eclipse.wazaabi.engine.bundled.nonosgi;

import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.bundled.converters.EDPBundledConverterFactory;
import org.eclipse.wazaabi.engine.edp.bundled.validators.EDPBundledValidatorFactory;
import org.eclipse.wazaabi.engine.edp.converters.BundledConverterFactory;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidatorFactory;

public class EDPBundledHelper {

	public static synchronized void init(Registry registry) {
		EDPHelper.init(registry);
		EDPHelper.addService(registry, BundledValidatorFactory.class,
				new EDPBundledValidatorFactory());
		EDPHelper.addService(registry, BundledConverterFactory.class,
				new EDPBundledConverterFactory());
	}

}
