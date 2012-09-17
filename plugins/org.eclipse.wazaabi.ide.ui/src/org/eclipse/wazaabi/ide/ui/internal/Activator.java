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

package org.eclipse.wazaabi.ide.ui.internal;

import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.wazaabi.ide.ui.PaletteContribution;
import org.eclipse.wazaabi.ide.ui.editors.ImagesUtils;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin implements ServiceListener {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.eclipse.wazaabi.ide.ui"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;
	private BundleContext context;

	private static PaletteContributionRegistry paletteContributionRegistry = null;

	public static PaletteContributionRegistry getPaletteContributionRegistry() {
		if (paletteContributionRegistry == null)
			paletteContributionRegistry = new PaletteContributionRegistry();
		return paletteContributionRegistry;
	}

	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
	 * )
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		this.context = context;
		plugin = this;
		context.addServiceListener(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
	 * )
	 */
	public void stop(BundleContext context) throws Exception {
		context.removeServiceListener(this);
		plugin = null;
		super.stop(context);
		this.context = null;
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	protected void initializeImageRegistry(ImageRegistry reg) {
		ImagesUtils.initializeImageRegistry(reg);
	}

	@SuppressWarnings("unchecked")
	public void serviceChanged(ServiceEvent ev) {
		
		@SuppressWarnings("rawtypes")
		ServiceReference sr = ev.getServiceReference();
		switch (ev.getType()) {
		case ServiceEvent.REGISTERED:
			if (isPaletteContribution(sr) && context != null)
				getPaletteContributionRegistry().addPaletteContribution(
						(PaletteContribution) context.getService(sr));
			break;
		case ServiceEvent.UNREGISTERING:
			if (isPaletteContribution(sr) && context != null)
				getPaletteContributionRegistry().removePaletteContribution(
						(PaletteContribution) context.getService(sr));
			break;
		}
		
	}

	@SuppressWarnings({ "rawtypes" })
	public static boolean isPaletteContribution(ServiceReference sr) {
		String objectClass[] = (String[]) sr.getProperty("objectClass");//$NON-NLS-1$
		for (String className : objectClass)
			if (PaletteContribution.class.getName().equals(className))
				return true;
		return false;
	}
}
