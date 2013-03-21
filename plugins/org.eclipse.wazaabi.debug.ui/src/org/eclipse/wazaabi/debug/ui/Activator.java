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

	public static final String DEBUG_PORT = "debugPort"; //$NON-NLS-1$

	static public final String MODEL_LOCATION = "modelLocation"; //$NON-NLS-1$

	private static BundleContext context;

	static BundleContext getContext() {
		return context;
	}

	private static ModelDisplayService modelDisplay = null;

	public static ModelDisplayService getModelDisplay() {
		return modelDisplay;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
	 * )
	 */
	public void start(BundleContext bundleContext) throws Exception {

		int debugPort = -1;
		try {
			String str = bundleContext.getProperty(DEBUG_PORT);
			if (str != null)
				debugPort = Integer.parseInt(bundleContext
						.getProperty(DEBUG_PORT));
		} catch (Exception e) {
			// NOTHING TO DO HERE
		}
		if (bundleContext.getProperty(MODEL_LOCATION) != null
				&& debugPort != -1) {
			modelDisplay = new ModelDisplayService(
					bundleContext.getProperty(MODEL_LOCATION), debugPort);
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
			logger.debug("...ending ModelDisplayService"); //$NON-NLS-1$
			modelDisplay.deactivate();
			modelDisplay = null;
		}
		Activator.context = null;
	}

}
