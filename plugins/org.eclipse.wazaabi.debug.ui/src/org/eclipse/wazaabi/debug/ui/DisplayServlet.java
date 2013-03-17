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

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DisplayServlet extends HttpServlet {

	private final static Logger logger = LoggerFactory
			.getLogger(DisplayServlet.class);

	private static final long serialVersionUID = 1L;

	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		logger.debug("got GET request");
		resp.setContentType("text/plain");
		resp.getWriter().write("Hello from the cloud!");
	}

	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		logger.debug("received a post request");
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		byte[] buffer = new byte[1024];
		int length = 0;
		while ((length = req.getInputStream().read(buffer)) != -1)
			baos.write(buffer, 0, length);

		if (Activator.getModelDisplay() != null)
			Activator.getModelDisplay().processCommand(
					new String(baos.toByteArray()));
		// Jetty seems to need to return something
		resp.setContentType("text/plain");
		resp.getWriter().write("");

	}
}