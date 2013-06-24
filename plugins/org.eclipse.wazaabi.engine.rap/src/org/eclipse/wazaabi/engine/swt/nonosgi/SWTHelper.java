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

import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.events.EventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.engine.rap.events.RapEventAdapterFactory;
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTCommonsHelper;

public class SWTHelper {

	public synchronized static void init(Registry registry) {
		SWTCommonsHelper.init(registry);
		EDPHelper.addService(registry, EventAdapterFactory.class,
				new RapEventAdapterFactory());
	}
}
