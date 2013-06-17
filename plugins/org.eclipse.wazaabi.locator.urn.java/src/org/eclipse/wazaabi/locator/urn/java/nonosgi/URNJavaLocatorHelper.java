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

package org.eclipse.wazaabi.locator.urn.java.nonosgi;

import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.locator.urn.java.codelocators.UrnJavaCodeLocator;

public class URNJavaLocatorHelper {

	public static synchronized void init(Registry registry) {
		EDPHelper.init(registry);

		EDPHelper.addService(registry, ICodeLocator.class,
				new UrnJavaCodeLocator());
	}

}
