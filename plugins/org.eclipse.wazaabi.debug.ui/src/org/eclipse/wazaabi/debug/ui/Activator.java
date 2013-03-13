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

package org.eclipse.wazaabi.debug.ui;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Activator implements BundleActivator {

	private final static Logger logger = LoggerFactory
			.getLogger(Activator.class);

	static public final String DISPLAY_SERVICE_PORT = "displayService.port"; //$NON-NLS-1$

	private static BundleContext context;

	static BundleContext getContext() {
		return context;
	}

	private ModelDisplayService modelDisplay = null;

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
	 * )
	 */
	public void start(BundleContext bundleContext) throws Exception {
		String displayServicePort = bundleContext
				.getProperty(DISPLAY_SERVICE_PORT);
		int port = -1;
		if (displayServicePort != null)
			try {
				port = Integer.parseInt(displayServicePort);
			} catch (NumberFormatException e) {
				// NOTHING TO DO HERE
			}
		if (port != -1) {
			logger.debug("Starting ModelDisplayService listening on port {}",
					port);
			modelDisplay = new ModelDisplayService(port);
			modelDisplay.activate();
		}
		Activator.context = bundleContext;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		if (modelDisplay != null) {
			logger.debug("ending ModelDisplayService");
			modelDisplay.deactivate();
			modelDisplay = null;
		}
		Activator.context = null;
	}
}
