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

package org.eclipse.wazaabi.engine.core.nonosgi;

import org.eclipse.wazaabi.engine.core.annotations.factories.AnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.annotations.factories.CoreAnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.editparts.factories.CoreEditPartFactory;
import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.events.CoreEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.core.stylerules.factories.CoreStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.stylerules.factories.StyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.events.EventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.nonosgi.EDPHelper;
import org.eclipse.wazaabi.mm.core.CorePackage;

public class CoreHelper {

	/**
	 * Initializes the CoreSingletons class when called from a non osgi
	 * environment. Could be called more than once.
	 */
	public static synchronized void init(Registry registry) {
		EDPHelper.init(registry);

		EDPHelper.addService(registry, EditPartFactory.class,
				new CoreEditPartFactory());
		EDPHelper.addService(registry, StyleRuleManagerFactory.class,
				new CoreStyleRuleManagerFactory());
		EDPHelper.addService(registry, AnnotationManagerFactory.class,
				new CoreAnnotationManagerFactory());
		EDPHelper.addService(registry, EventHandlerAdapterFactory.class,
				new CoreEventHandlerAdapterFactory());

		CorePackage.eINSTANCE.eClass();

	}

}
