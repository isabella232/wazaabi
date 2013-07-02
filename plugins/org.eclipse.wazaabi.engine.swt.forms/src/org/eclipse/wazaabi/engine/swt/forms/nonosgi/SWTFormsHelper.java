/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.forms.nonosgi;

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.stylerules.factories.StyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTCommonsHelper;
import org.eclipse.wazaabi.engine.swt.forms.editparts.SWTFormsEditPartFactory;
import org.eclipse.wazaabi.engine.swt.forms.stylerules.factories.SWTFormsStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.swt.forms.views.SWTFormsWidgetViewFactory;

public class SWTFormsHelper {

	public static void init(Registry registry) {

		EDPHelper.addService(registry, WidgetViewFactory.class,
				new SWTFormsWidgetViewFactory());
		EDPHelper.addService(registry, EditPartFactory.class,
				new SWTFormsEditPartFactory());
		EDPHelper.addService(registry, StyleRuleManagerFactory.class,
				new SWTFormsStyleRuleManagerFactory());
		SWTCommonsHelper.init(registry);

	}

}
