/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.core.themes.nonosgi;

import org.eclipse.wazaabi.engine.core.nonosgi.CoreHelper;
import org.eclipse.wazaabi.mm.core.themes.Themes.CoreThemesPackage;

public class CoreThemesHelper {

	private static boolean neverCalled = true;

	/**
	 * Initializes the CoreSingletons class when called from a non osgi
	 * environment. Could be called more than once.
	 */
	public static synchronized void init() {
		if (!neverCalled)
			return;
		CoreHelper.init();
//		CoreSingletons.getComposedAnnotationManagerFactory()
//				.addAnnotationManagerFactory(
//						new CoreThemesAnnotationManagerFactory());
		CoreThemesPackage.eINSTANCE.eClass();
		neverCalled = false;
	}

}
