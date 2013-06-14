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

import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;

public class EDPBundledHelper {

	private static boolean neverCalled = true;

	/**
	 * Initializes the Registry when called from a non osgi environment. Could
	 * be called more than once.
	 */
	public static synchronized void init() {
		if (!neverCalled)
			return;
		EDPHelper.init();

//		EDPSingletons.getComposedBundledValidatorFactory()
//		.addBundledValidatorFactory(new EDPBundledValidatorFactory());
//
//		EDPSingletons.getComposedBundledConverterFactory()
//				.addBundledConverterFactory(new EDPBundledConverterFactory());
		neverCalled = false;
	}

}
