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

	static public final String START_DISPLAY_SERVICE = "StartDisplayService"; //$NON-NLS-1$

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
		if (bundleContext.getProperty(START_DISPLAY_SERVICE) != null) {
			logger.debug("{} defined, starting ModelDisplayService",
					START_DISPLAY_SERVICE);
			modelDisplay = new ModelDisplayService();
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
