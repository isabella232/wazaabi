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

package org.eclipse.wazaabi.ui.runtime.parts.nonosgi;

import org.eclipse.wazaabi.engine.core.CoreSingletons;
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTHelper;
import org.eclipse.wazaabi.ui.model.parts.PartsPackage;
import org.eclipse.wazaabi.ui.runtime.parts.editparts.PartsEditPartFactory;
import org.eclipse.wazaabi.ui.runtime.parts.views.PartsWidgetViewFactory;

public class PartsHelper {

	private static boolean neverCalled = true;

	public synchronized static void init() {
		if (!neverCalled)
			return;
		SWTHelper.init();

		CoreSingletons.getComposedEditPartFactory().addEditPartFactory(
				new PartsEditPartFactory());
		CoreSingletons.getComposedWidgetViewFactory().addWidgetViewFactory(
				new PartsWidgetViewFactory());
		PartsPackage.eINSTANCE.eClass();
		neverCalled = false;
	}
}
