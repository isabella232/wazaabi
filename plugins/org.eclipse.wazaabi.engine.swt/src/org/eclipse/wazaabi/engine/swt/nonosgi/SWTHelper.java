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

package org.eclipse.wazaabi.engine.swt.nonosgi;

import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.wazaabi.engine.core.CoreSingletons;
import org.eclipse.wazaabi.engine.core.nonosgi.CoreHelper;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.swt.editparts.SWTEditPartFactory;
import org.eclipse.wazaabi.engine.swt.editparts.stylerules.factories.SWTSpecificStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.swt.events.SWTEventAdapterFactory;
import org.eclipse.wazaabi.engine.swt.events.SWTEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.swt.views.SWTWidgetViewFactory;

public class SWTHelper {

	private static boolean neverCalled = true;

	public synchronized static void init() {
		if (!neverCalled)
			return;
		CoreHelper.init();

		EDPSingletons.getComposedEventAdapterFactory().addEventAdapterFactory(
				new SWTEventAdapterFactory());
		EDPSingletons.getComposedEventHandlerAdapterFactory()
				.addEventHandlerAdapterFactory(
						new SWTEventHandlerAdapterFactory());

		CoreSingletons.getComposedEditPartFactory().addEditPartFactory(
				new SWTEditPartFactory());
		CoreSingletons.getComposedWidgetViewFactory().addWidgetViewFactory(
				new SWTWidgetViewFactory());
		CoreSingletons.getComposedStyleRuleManagerFactory()
				.addStyleRuleManagerFactory(
						new SWTSpecificStyleRuleManagerFactory());
		SWTStylesPackage.eINSTANCE.eClass();
		neverCalled = false;
	}
}
