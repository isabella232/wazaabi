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

package org.eclipse.wazaabi.engine.edp.nonosgi;

import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.ComposedCodeLocatorImpl;
import org.eclipse.wazaabi.engine.edp.converters.internal.ComposedBundledConverterFactoryImpl;
import org.eclipse.wazaabi.engine.edp.events.EDPEventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.EDPEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.internal.ComposedEventAdapterFactoryImpl;
import org.eclipse.wazaabi.engine.edp.events.internal.ComposedEventHandlerAdapterFactoryImpl;
import org.eclipse.wazaabi.engine.edp.executables.EDPExecutableAdapterFactory;
import org.eclipse.wazaabi.engine.edp.executables.internal.ComposedExecutableAdapterFactoryImpl;
import org.eclipse.wazaabi.engine.edp.internal.RegistryImpl;
import org.eclipse.wazaabi.engine.edp.validators.internal.ComposedBundledValidatorFactoryImpl;

public class EDPHelper {

	private static boolean neverCalled = true;

	/**
	 * Initializes the Registry when called from a non osgi environment. Could
	 * be called more than once.
	 */
	public static synchronized void init() {
		if (!neverCalled)
			return;
		EDPSingletons.setRegistry(new RegistryImpl());
		EDPSingletons
				.setComposedEventAdapterFactory(new ComposedEventAdapterFactoryImpl());
		EDPSingletons
				.setComposedEventHandlerAdapterFactory(new ComposedEventHandlerAdapterFactoryImpl());
		EDPSingletons.getComposedEventHandlerAdapterFactory()
				.addEventHandlerAdapterFactory(
						new EDPEventHandlerAdapterFactory());
		EDPSingletons.getComposedEventAdapterFactory().addEventAdapterFactory(
				new EDPEventAdapterFactory());
		EDPSingletons.setComposedCodeLocator(new ComposedCodeLocatorImpl());
		EDPSingletons
				.setComposedExecutableAdapterFactory(new ComposedExecutableAdapterFactoryImpl());
		EDPSingletons.getComposedExecutableAdapterFactory()
				.addExecutableAdapterFactory(new EDPExecutableAdapterFactory());
		EDPSingletons
				.setComposedBundledConverterFactory(new ComposedBundledConverterFactoryImpl());
		EDPSingletons
				.setComposedBundledValidatorFactory(new ComposedBundledValidatorFactoryImpl());

		neverCalled = false;
	}

}
