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

package org.eclipse.wazaabi.engine.core.internal;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

	// The shared instance
	private static Activator plugin;

	// The plug-in ID
	public static final String PLUGIN_ID = "org.eclipse.wazaabi.engine.core"; //$NON-NLS-1$

	public static Activator getDefault() {
		return plugin;
	}

	private BundleContext context;

	public void start(BundleContext context) throws Exception {
		this.context = context;
		plugin = this;
	}

	public void stop(BundleContext context) throws Exception {
		this.context = null;
		plugin = null;

	}
}
