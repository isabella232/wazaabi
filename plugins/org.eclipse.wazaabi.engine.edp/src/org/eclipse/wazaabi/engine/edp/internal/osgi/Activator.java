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

package org.eclipse.wazaabi.engine.edp.internal.osgi;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;

public class Activator implements BundleActivator, ServiceListener {

	// The shared instance
	private static Activator plugin;

	// The plug-in ID
	public static final String PLUGIN_ID = "org.eclipse.wazaabi.engine.edp"; //$NON-NLS-1$

	public static Activator getDefault() {
		return plugin;
	}

	private BundleContext context;

	public BundleContext getContext() {
		return context;
	}

//	private ServiceTracker logTracker;

	public void start(BundleContext context) throws Exception {
		this.context = context;
		plugin = this;
//		context.addServiceListener(this);
	}

	public void stop(BundleContext context) throws Exception {
		this.context = null;
		plugin = null;
//		if (logTracker != null) {
//			logTracker.close();
//			logTracker = null;
//		}
	}

	@SuppressWarnings("unchecked")
	public void serviceChanged(ServiceEvent ev) {
		// @SuppressWarnings("rawtypes")
		// ServiceReference sr = ev.getServiceReference();
		// if (sr == null || !sr.getBundle().equals(context.getBundle()))
		// return;
		//
		//		final String componentName = (String) sr.getProperty("component.name"); //$NON-NLS-1$
		// switch (ev.getType()) {
		// case ServiceEvent.REGISTERED:
		// if (Registry.class.getName().equals(componentName))
		// EDPSingletons.setRegistry((Registry) context.getService(sr));
		// else if (ComposedEventHandlerAdapterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons
		// .setComposedEventHandlerAdapterFactory((ComposedEventHandlerAdapterFactory)
		// context
		// .getService(sr));
		// else if (ComposedEventAdapterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons
		// .setComposedEventAdapterFactory((ComposedEventAdapterFactory) context
		// .getService(sr));
		// else if (ComposedCodeLocator.class.getName().equals(componentName))
		// EDPSingletons
		// .setComposedCodeLocator((ComposedCodeLocator) context
		// .getService(sr));
		// else if (ComposedExecutableAdapterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons
		// .setComposedExecutableAdapterFactory((ComposedExecutableAdapterFactory)
		// context
		// .getService(sr));
		// else if (ComposedBundledConverterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons
		// .setComposedBundledConverterFactory((ComposedBundledConverterFactory)
		// context
		// .getService(sr));
		// else if (ComposedBundledValidatorFactory.class.getName().equals(
		// componentName))
		// EDPSingletons
		// .setComposedBundledValidatorFactory((ComposedBundledValidatorFactory)
		// context
		// .getService(sr));
		// break;
		// case ServiceEvent.UNREGISTERING:
		// if (Registry.class.getName().equals(componentName))
		// EDPSingletons.setRegistry(null);
		// else if (ComposedEventHandlerAdapterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons.setComposedEventHandlerAdapterFactory(null);
		// else if (ComposedEventAdapterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons.setComposedEventAdapterFactory(null);
		// else if (ComposedCodeLocator.class.getName().equals(componentName))
		// EDPSingletons.setComposedCodeLocator(null);
		// else if (ComposedExecutableAdapterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons.setComposedExecutableAdapterFactory(null);
		// else if (ComposedBundledConverterFactory.class.getName().equals(
		// componentName))
		// EDPSingletons.setComposedBundledConverterFactory(null);
		// else if (ComposedBundledValidatorFactory.class.getName().equals(
		// componentName))
		// EDPSingletons.setComposedBundledValidatorFactory(null);
		// break;
		// }
	}
}
