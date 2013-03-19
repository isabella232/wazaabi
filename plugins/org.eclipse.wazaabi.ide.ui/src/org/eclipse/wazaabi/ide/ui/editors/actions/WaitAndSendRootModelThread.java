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

package org.eclipse.wazaabi.ide.ui.editors.actions;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;

import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WaitAndSendRootModelThread extends Thread {

	private final static Logger logger = LoggerFactory
			.getLogger(WaitAndSendRootModelThread.class);

	private final AbstractComponent rootModel;
	private final String server;
	private final int port;
	public static final String SERVLET_NAME = "displayServlet"; //$NON-NLS-1$

	public static int findFreePort() {
		int port;
		try {
			ServerSocket socket = new ServerSocket(0);
			port = socket.getLocalPort();
			socket.close();
		} catch (Exception e) {
			port = -1;
		}
		return port;
	}

	public WaitAndSendRootModelThread(AbstractComponent rootModel,
			String server, int port) {
		this.rootModel = rootModel;
		this.server = server;
		this.port = port;
	}

	protected AbstractComponent getRootModel() {
		return rootModel;
	}

	protected String getServer() {
		return server;
	}

	protected int getPort() {
		return port;
	}

	protected boolean isServerListening(String server, int port,
			int sleepingDelay, int nbrLoops) {
		boolean isServerListening = false;
		for (int i = 0; i < nbrLoops; i++) {
			final Socket sock = new Socket();
			try {
				sock.connect(new InetSocketAddress(server, port), 500);
				if (sock.isConnected())
					isServerListening = true;
			} catch (IOException e) {
				logger.debug("no socket available on {}:{}", new Object[] {
						server, port });
			} finally {
				try {
					sock.close();
				} catch (IOException e) {
				}
			}
			if (isServerListening)
				break;
			try {
				sleep(sleepingDelay);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		return isServerListening;
	}

	public void run() {

		if (isServerListening(getServer(), getPort(), 500, 30)) {
			sendAbstractComponent(getRootModel(), server, port);
		}
	}

	protected void sendAbstractComponent(AbstractComponent rootModel,
			String server, int port) {
		URL url = null;
		try {
			url = new URL("http://" + server + ":" + port + "/" + SERVLET_NAME);
		} catch (MalformedURLException ex) {
			// NOTHING TO DO HERE
		}
		HttpURLConnection urlConn = null;
		try {
			urlConn = (HttpURLConnection) url.openConnection();
		} catch (IOException e) {
			logger.error("{} {}", new Object[] { e.getMessage(), e.getCause() });
		}

		urlConn.setDoInput(true);
		urlConn.setDoOutput(true);
		urlConn.setUseCaches(false);

		try {
			urlConn.setRequestMethod("POST");
		} catch (ProtocolException e) {
			logger.error("{} {}", new Object[] { e.getMessage(), e.getCause() });
		}

		try {
			urlConn.connect();
		} catch (IOException e) {
			logger.error("{} {}", new Object[] { e.getMessage(), e.getCause() });
		}

		DataOutputStream output = null;

		try {
			output = new DataOutputStream(urlConn.getOutputStream());
		} catch (IOException e) {
			logger.error("{} {}", new Object[] { e.getMessage(), e.getCause() });
		}

		// Specify the content type if needed.
		// urlConn.setRequestProperty("Content-Type",
		// "application/x-www-form-urlencoded");

		XMIResource r = new XMIResourceImpl();
		r.getContents().add(rootModel);

		// Send the request data.
		try {
			r.save(output, null);
			output.flush();
			output.close();
		} catch (IOException e) {
			logger.error("{} {}", new Object[] { e.getMessage(), e.getCause() });
		}

		ByteArrayOutputStream bout = new ByteArrayOutputStream();
		try {
			int DEFAULT_BUFFER_SIZE = 1024;
			byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
			int n = 0;
			int length = 0;

			while ((n = urlConn.getInputStream().read(buffer, 0,
					DEFAULT_BUFFER_SIZE)) > 0) {
				bout.write(buffer, 0, n);
				length += n;
			}
			logger.debug(new String(bout.toByteArray(), 0, length, "UTF-8"));
		} catch (IOException e) {
			logger.error("{} {}", new Object[] { e.getMessage(), e.getCause() });
		} finally {
			try {
				urlConn.getInputStream();
			} catch (IOException e) {
				logger.error("{} {}",
						new Object[] { e.getMessage(), e.getCause() });
			}
		}
	}
}
