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

import org.eclipse.wazaabi.engine.core.CoreSingletons;
import org.eclipse.wazaabi.engine.core.annotations.factories.ComposedAnnotationManagerFactory;
import org.eclipse.wazaabi.engine.core.editparts.factories.ComposedEditPartFactory;
import org.eclipse.wazaabi.engine.core.stylerules.factories.ComposedStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.views.factories.ComposedWidgetViewFactory;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;

public class Activator implements BundleActivator, ServiceListener {

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
		context.addServiceListener(this);
	}

	public void stop(BundleContext context) throws Exception {
		this.context = null;
		plugin = null;
	}

	public void serviceChanged(ServiceEvent ev) {
		final ServiceReference sr = ev.getServiceReference();
		if (sr == null || !sr.getBundle().equals(context.getBundle()))
			return;

		final String componentName = (String) sr.getProperty("component.name"); //$NON-NLS-1$

		switch (ev.getType()) {
		case ServiceEvent.REGISTERED:
			if (ComposedEditPartFactory.class.getName().equals(componentName))
				CoreSingletons
						.setComposedEditPartFactory((ComposedEditPartFactory) context
								.getService(sr));
			else if (ComposedWidgetViewFactory.class.getName().equals(
					componentName))
				CoreSingletons
						.setComposedWidgetViewFactory((ComposedWidgetViewFactory) context
								.getService(sr));
			else if (ComposedStyleRuleManagerFactory.class.getName().equals(
					componentName))
				CoreSingletons
						.setComposedStyleRuleManagerFactory((ComposedStyleRuleManagerFactory) context
								.getService(sr));
			else if (ComposedAnnotationManagerFactory.class.getName().equals(
					componentName))
				CoreSingletons
						.setComposedAnnotationManagerFactory((ComposedAnnotationManagerFactory) context
								.getService(sr));
			break;
		case ServiceEvent.UNREGISTERING:
			if (ComposedEditPartFactory.class.getName().equals(componentName))
				CoreSingletons.setComposedEditPartFactory(null);
			else if (ComposedWidgetViewFactory.class.getName().equals(
					componentName))
				CoreSingletons.setComposedWidgetViewFactory(null);
			else if (ComposedStyleRuleManagerFactory.class.getName().equals(
					componentName))
				CoreSingletons.setComposedStyleRuleManagerFactory(null);
			else if (ComposedAnnotationManagerFactory.class.getName().equals(
					componentName))
				CoreSingletons.setComposedAnnotationManagerFactory(null);
			break;
		}
	}
}
