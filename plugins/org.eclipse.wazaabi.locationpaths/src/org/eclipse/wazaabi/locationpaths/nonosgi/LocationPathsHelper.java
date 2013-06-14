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

package org.eclipse.wazaabi.locationpaths.nonosgi;

import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;

public class LocationPathsHelper {

	private static boolean neverCalled = true;

	public synchronized static void init() {
		if (!neverCalled)
			return;
		EDPHelper.init();
//		EDPSingletons.getRegistry().addPointersEvaluator(
//				new PointersEvaluatorImpl());
		neverCalled = false;
	}

}
