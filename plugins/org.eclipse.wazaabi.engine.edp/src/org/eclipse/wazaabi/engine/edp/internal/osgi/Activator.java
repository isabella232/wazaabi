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

import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.Logger;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.coderesolution.ComposedCodeLocator;
import org.eclipse.wazaabi.engine.edp.converters.ComposedBundledConverterFactory;
import org.eclipse.wazaabi.engine.edp.events.ComposedEventAdapterFactory;
import org.eclipse.wazaabi.engine.edp.events.ComposedEventHandlerAdapterFactory;
import org.eclipse.wazaabi.engine.edp.executables.ComposedExecutableAdapterFactory;
import org.eclipse.wazaabi.engine.edp.validators.ComposedBundledValidatorFactory;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;
import org.osgi.service.log.LogService;
import org.osgi.util.tracker.ServiceTracker;

public class Activator implements BundleActivator, ServiceListener {

	// The shared instance
	private static Activator plugin;

	// The plug-in ID
	public static final String PLUGIN_ID = "org.eclipse.wazaabi.engine.edp"; //$NON-NLS-1$

	public static Activator getDefault() {
		return plugin;
	}

	public static void log(String name, int level, String message,
			Throwable exception) {
		LogService logService = Activator.getDefault().getLogService();
		if (logService != null)
			switch (level) {
			case Logger.DEBUG:
				logService.log(LogService.LOG_DEBUG, name + ": " + message,
						exception);
				break;
			case Logger.ERROR:
				logService.log(LogService.LOG_ERROR, name + ": " + message,
						exception);
				break;
			case Logger.INFO:
				logService.log(LogService.LOG_INFO, name + ": " + message,
						exception);
				break;
			case Logger.WARNING:
				logService.log(LogService.LOG_WARNING, name + ": " + message,
						exception);
				break;
			}
	}

	private BundleContext context;
	private ServiceTracker logTracker;

	public LogService getLogService() {
		LogService logService = null;
		if (logTracker != null) {
			logService = (LogService) logTracker.getService();
		} else {
			if (context != null) {
				logTracker = new ServiceTracker(context,
						LogService.class.getName(), null);
				logTracker.open();
				logService = (LogService) logTracker.getService();
			}
		}
		if (logService == null) {
			logService = new LogService() {
				public void log(int level, String message) {
					log(null, level, message, null);
				}

				public void log(int level, String message, Throwable exception) {
					log(null, level, message, exception);
				}

				public void log(ServiceReference sr, int level, String message) {
					log(sr, level, message, null);
				}

				public void log(ServiceReference sr, int level, String message,
						Throwable exception) {
					if (level == LogService.LOG_ERROR) {
						System.err.print("ERROR: "); //$NON-NLS-1$
					} else if (level == LogService.LOG_WARNING) {
						System.err.print("WARNING: "); //$NON-NLS-1$
					} else if (level == LogService.LOG_INFO) {
						System.err.print("INFO: "); //$NON-NLS-1$
					} else if (level == LogService.LOG_DEBUG) {
						System.err.print("DEBUG: "); //$NON-NLS-1$
					} else {
						System.err.print("log level " + level + ": "); //$NON-NLS-1$ //$NON-NLS-2$
					}
					System.err.println(message);
					if (exception != null) {
						exception.printStackTrace(System.err);
					}
				}
			};
		}
		return logService;
	}

	public void start(BundleContext context) throws Exception {
		this.context = context;
		plugin = this;
		context.addServiceListener(this);
	}

	public void stop(BundleContext context) throws Exception {
		this.context = null;
		plugin = null;
		if (logTracker != null) {
			logTracker.close();
			logTracker = null;
		}
	}

	@SuppressWarnings("unchecked")
	public void serviceChanged(ServiceEvent ev) {
		@SuppressWarnings("rawtypes")
		ServiceReference sr = ev.getServiceReference();
		if (sr == null || !sr.getBundle().equals(context.getBundle()))
			return;

		final String componentName = (String) sr.getProperty("component.name"); //$NON-NLS-1$
		switch (ev.getType()) {
		case ServiceEvent.REGISTERED:
			if (Registry.class.getName().equals(componentName))
				EDPSingletons.setRegistry((Registry) context.getService(sr));
			else if (ComposedEventHandlerAdapterFactory.class.getName().equals(
					componentName))
				EDPSingletons
						.setComposedEventHandlerAdapterFactory((ComposedEventHandlerAdapterFactory) context
								.getService(sr));
			else if (ComposedEventAdapterFactory.class.getName().equals(
					componentName))
				EDPSingletons
						.setComposedEventAdapterFactory((ComposedEventAdapterFactory) context
								.getService(sr));
			else if (ComposedCodeLocator.class.getName().equals(componentName))
				EDPSingletons
						.setComposedCodeLocator((ComposedCodeLocator) context
								.getService(sr));
			else if (ComposedExecutableAdapterFactory.class.getName().equals(
					componentName))
				EDPSingletons
						.setComposedExecutableAdapterFactory((ComposedExecutableAdapterFactory) context
								.getService(sr));
			else if (ComposedBundledConverterFactory.class.getName().equals(
					componentName))
				EDPSingletons
						.setComposedBundledConverterFactory((ComposedBundledConverterFactory) context
								.getService(sr));
			else if (ComposedBundledValidatorFactory.class.getName().equals(
					componentName))
				EDPSingletons
						.setComposedBundledValidatorFactory((ComposedBundledValidatorFactory) context
								.getService(sr));
			break;
		case ServiceEvent.UNREGISTERING:
			if (Registry.class.getName().equals(componentName))
				EDPSingletons.setRegistry(null);
			else if (ComposedEventHandlerAdapterFactory.class.getName().equals(
					componentName))
				EDPSingletons.setComposedEventHandlerAdapterFactory(null);
			else if (ComposedEventAdapterFactory.class.getName().equals(
					componentName))
				EDPSingletons.setComposedEventAdapterFactory(null);
			else if (ComposedCodeLocator.class.getName().equals(componentName))
				EDPSingletons.setComposedCodeLocator(null);
			else if (ComposedExecutableAdapterFactory.class.getName().equals(
					componentName))
				EDPSingletons.setComposedExecutableAdapterFactory(null);
			else if (ComposedBundledConverterFactory.class.getName().equals(
					componentName))
				EDPSingletons.setComposedBundledConverterFactory(null);
			else if (ComposedBundledValidatorFactory.class.getName().equals(
					componentName))
				EDPSingletons.setComposedBundledValidatorFactory(null);
			break;
		}
	}
}
