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

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.ui.model.parts.PartsPackage;
import org.eclipse.wazaabi.ui.runtime.parts.editparts.PartsEditPartFactory;
import org.eclipse.wazaabi.ui.runtime.parts.views.PartsWidgetViewFactory;

public class PartsHelper {

	public synchronized static void init(Registry registry) {
		SWTHelper.init(registry);

		EDPHelper.addService(registry, EditPartFactory.class,
				new PartsEditPartFactory());
		EDPHelper.addService(registry, WidgetViewFactory.class,
				new PartsWidgetViewFactory());
		PartsPackage.eINSTANCE.eClass();
	}
}
