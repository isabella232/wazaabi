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

import org.eclipse.wazaabi.engine.core.celleditors.factories.CellEditorFactory;
import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.nonosgi.CoreHelper;
import org.eclipse.wazaabi.engine.core.stylerules.factories.StyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.engine.rap.events.RapEventAdapterFactory;
import org.eclipse.wazaabi.engine.swt.commons.celleditors.factories.SWTCellEditorFactory;
import org.eclipse.wazaabi.engine.swt.commons.editparts.SWTEditPartFactory;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.factories.SWTSpecificStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.swt.commons.events.SWTEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetViewFactory;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class SWTHelper {

	public synchronized static void init(Registry registry) {
		CoreHelper.init(registry);
		EDPHelper.addService(registry, EventAdapterFactory.class,
				new RapEventAdapterFactory());
		EDPHelper.addService(registry, EventHandlerAdapterFactory.class,
				new SWTEventHandlerAdapterFactory());
		EDPHelper.addService(registry, EditPartFactory.class,
				new SWTEditPartFactory());
		EDPHelper.addService(registry, WidgetViewFactory.class,
				new SWTWidgetViewFactory());
		EDPHelper.addService(registry, StyleRuleManagerFactory.class,
				new SWTSpecificStyleRuleManagerFactory());
		EDPHelper.addService(registry, CellEditorFactory.class,
				new SWTCellEditorFactory());
		SWTStylesPackage.eINSTANCE.eClass();
	}
}
