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

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.service.http.HttpService;
import org.osgi.util.tracker.ServiceTracker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings("rawtypes")
public class HttpServiceTracker extends ServiceTracker {

	public static final String SERVLET_NAME = "displayServlet";

	private final static Logger logger = LoggerFactory
			.getLogger(HttpServiceTracker.class);

	@SuppressWarnings("unchecked")
	public HttpServiceTracker(BundleContext context) {
		super(context, HttpService.class.getName(), null);
	}

	@SuppressWarnings("unchecked")
	public Object addingService(ServiceReference reference) {
		HttpService httpService = (HttpService) super.addingService(reference);
		if (httpService == null)
			return null;

		try {
			logger.debug("Registering servlet at /{}", SERVLET_NAME);
			httpService.registerServlet("/" + SERVLET_NAME,
					new DisplayServlet(), null, null);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return httpService;
	}

	@SuppressWarnings("unchecked")
	public void removedService(ServiceReference reference, Object service) {
		HttpService httpService = (HttpService) service;

		logger.debug("Unregistering servlet at /{}", SERVLET_NAME);
		httpService.unregister("/" + SERVLET_NAME);

		super.removedService(reference, service);
	}

}