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

import org.eclipse.wazaabi.engine.core.annotations.factories.AnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.nonosgi.CoreHelper;
import org.eclipse.wazaabi.engine.core.themes.annotation.factories.CoreThemesAnnotationManagerFactory;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.mm.core.themes.Themes.CoreThemesPackage;

public class CoreThemesHelper {

	public static synchronized void init(Registry registry) {
		CoreHelper.init(registry);
		EDPHelper.addService(registry, AnnotationManagerFactory.class,
				new CoreThemesAnnotationManagerFactory());
		CoreThemesPackage.eINSTANCE.eClass();
	}

}
