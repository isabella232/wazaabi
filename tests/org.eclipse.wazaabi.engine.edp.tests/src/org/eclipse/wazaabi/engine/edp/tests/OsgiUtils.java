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

package org.eclipse.wazaabi.engine.edp.tests;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.runtime.adaptor.EclipseStarter;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

@SuppressWarnings("restriction")
public final class OsgiUtils {

	/**
	 * Launches the osgi framework
	 * 
	 * @return
	 */
	public static final BundleContext launchOsgiFwk() {

		String[] equinoxArgs = { "-clean", "-console"/*, "1234", "-noExit" */};
		BundleContext context = null;
		try {
			context = EclipseStarter.startup(equinoxArgs, null);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return context;
	}

	public static final void stopOsgiFwk() {
		try {
			EclipseStarter.shutdown();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Installs a bundle specified by its jar name into the given bundle
	 * context.
	 * 
	 * @param bundleContext
	 *            The bundle context where to install the bundle
	 * @param jarName
	 *            the file name of a jar located in the classpath known by the
	 *            given <code>ClassLoader</code>
	 * @param classLoader
	 *            Used to find the jar using <code>getResourceAsStream</code>
	 * @return
	 */
	public final static Bundle installBundle(BundleContext bundleContext,
			String location, String jarName, ClassLoader classLoader) {

		if (bundleContext == null || classLoader == null)
			return null;
		try {
			InputStream in = classLoader.getResourceAsStream(jarName);

			Bundle bundle = bundleContext.installBundle(location, in);
			in.close();
			return bundle;
		} catch (BundleException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	public static Bundle installAndStartBundle(BundleContext context,
			String location, ClassLoader classLoader, String bundleFileName) {
		Bundle bundle = OsgiUtils.installBundle(context, location,
				bundleFileName, classLoader);
		if (bundle == null)
			return null;
		try {
			bundle.start();
		} catch (BundleException e2) {
			e2.printStackTrace();
		}
		// System.out.println(bundle.getSymbolicName() + ": "
		// + getBundleState(bundle.getState()));
		return bundle;
	}

	public static String prettyPrintBundleState(int bundleState) {
		switch (bundleState) {
		case Bundle.ACTIVE:
			return "ACTIVE";//$NON-NLS-1$
		case Bundle.INSTALLED:
			return "INSTALLED";//$NON-NLS-1$
		case Bundle.RESOLVED:
			return "RESOLVED";//$NON-NLS-1$
		case Bundle.STARTING:
			return "STARTING";//$NON-NLS-1$
		case Bundle.STOPPING:
			return "STOPPING";//$NON-NLS-1$
		case Bundle.UNINSTALLED:
			return "UNINSTALLED";//$NON-NLS-1$
		}
		return "";//$NON-NLS-1$
	}
}
