package org.eclipse.wazaabi.engine.core.themes;

import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleActivator;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator implements BundleActivator {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.eclipse.wazaabi.engine.core.themes"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;
	
	/**
	 * The constructor
	 */
	public Activator() {
	}


	public void start(BundleContext context) throws Exception {
		plugin = this;
	}


	public void stop(BundleContext context) throws Exception {
		plugin = null;
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

}
