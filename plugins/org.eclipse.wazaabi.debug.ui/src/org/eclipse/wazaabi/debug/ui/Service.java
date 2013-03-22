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

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

public class Service extends Thread {

	private static final int BUFFER_MAX = 1024;
	private final int port;
	private final ModelDisplayService displayService;

	public Service(ModelDisplayService displayService, int port) {
		this.displayService = displayService;
		this.port = port;
	}

	public int getPort() {
		return port;
	}

	@Override
	public void interrupt() {
		super.interrupt();
	}

	@Override
	public boolean isInterrupted() {
		return super.isInterrupted();
	}

	@Override
	public void run() {
		ByteBuffer byteBuffer = ByteBuffer.allocateDirect(BUFFER_MAX);
		ServerSocketChannel ssc = null;
		try {
			ssc = ServerSocketChannel.open();
		} catch (IOException e) {
			e.printStackTrace();
		}
		if (ssc != null) {
			try {
				ssc.socket().bind(new InetSocketAddress(getPort()));
			} catch (IOException e) {
				e.printStackTrace();
			}
			try {
				ssc.configureBlocking(false);
			} catch (IOException e) {
				e.printStackTrace();
			}

			while (true) {

				SocketChannel sc = null;
				try {
					sc = ssc.accept();
				} catch (IOException e1) {
					e1.printStackTrace();
				}

				if (sc == null) {
					// no connections, snooze a while
					try {
						Thread.sleep(500);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				} else {
					int len = -1;
					try {
						len = sc.read(byteBuffer);
					} catch (IOException e1) {
						e1.printStackTrace();
					}
					byte[] buf = new byte[len];
					byteBuffer.rewind();
					byteBuffer.get(buf, 0, len);
					if (displayService.isActive())
						displayService.processCommand(new String(buf));
					if (sc != null)
						try {
							sc.close();
						} catch (IOException e) {
							e.printStackTrace();
						}
				}
			}
		}

	}
}
