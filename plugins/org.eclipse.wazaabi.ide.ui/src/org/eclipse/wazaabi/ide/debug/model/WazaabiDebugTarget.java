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

package org.eclipse.wazaabi.ide.debug.model;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IThread;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WazaabiDebugTarget implements IDebugTarget {

	private final static Logger logger = LoggerFactory
			.getLogger(WazaabiDebugTarget.class);

	private final ILaunch launch;
	private final IProcess process;
	private final int requestPort;

	private IThread threads[] = null;

	public WazaabiDebugTarget(ILaunch launch, IProcess process, int requestPort) {
		this.launch = launch;
		this.process = process;
		this.requestPort = requestPort;
	}

	public String getModelIdentifier() {
		return "toto";
	}

	public int getRequestPort() {
		return requestPort;
	}

	public IDebugTarget getDebugTarget() {
		return this;
	}

	public ILaunch getLaunch() {
		return launch;
	}

	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		return null;
	}

	public boolean canTerminate() {
		return false;
	}

	public boolean isTerminated() {
		return false;
	}

	public void terminate() throws DebugException {

	}

	public boolean canResume() {
		return false;
	}

	public boolean canSuspend() {
		return false;
	}

	public boolean isSuspended() {
		return false;
	}

	public void resume() throws DebugException {
		sendRequest("suspend");
	}

	public void suspend() throws DebugException {
	}

	public void breakpointAdded(IBreakpoint breakpoint) {
	}

	public void breakpointRemoved(IBreakpoint breakpoint, IMarkerDelta delta) {
	}

	public void breakpointChanged(IBreakpoint breakpoint, IMarkerDelta delta) {
	}

	public boolean canDisconnect() {
		return false;
	}

	public void disconnect() throws DebugException {
	}

	public boolean isDisconnected() {
		return false;
	}

	public boolean supportsStorageRetrieval() {
		return false;
	}

	public IMemoryBlock getMemoryBlock(long startAddress, long length)
			throws DebugException {
		return null;
	}

	public IProcess getProcess() {
		return process;
	}

	public IThread[] getThreads() throws DebugException {
		// if (viewerThread == null) {
		// viewerThread = new ViewerThread(getLaunch(), this);
		// if (threads == null) {
		// threads = new IThread[1];
		// threads[0] = viewerThread;
		// }
		// }
		if (threads == null)
			threads = new IThread[] {};
		return threads;
	}

	public boolean hasThreads() throws DebugException {
		return true;
	}

	public String getName() throws DebugException {
		return "debug target name";
	}

	public boolean supportsBreakpoint(IBreakpoint breakpoint) {
		return false;
	}

	protected void sendRequest(String request) {

		Socket socket = new Socket();
		InetSocketAddress remoteAddress = new InetSocketAddress(
				getRequestPort());
		try {
			while (!socket.isConnected()) {
				try {
					socket.connect(remoteAddress, 500);
					try {
						Thread.sleep(500);
					} catch (InterruptedException e) {
					}
				} catch (IOException e) {
					// NOTHING TO DO HERE
				}
			}
			if (socket.isConnected()) {
				try {
					socket.getOutputStream().write(request.getBytes());
				} catch (IOException e) {
					logger.error("Unable to write to request socket \n{}\n{}",
							new Object[] { e.getMessage(), e.getCause() });
				}
			}
		} finally {
			if (socket != null)
				try {
					socket.close();
				} catch (IOException e) {
					logger.error("Unable to close request socket \n{}\n{}",
							new Object[] { e.getMessage(), e.getCause() });
				}
		}
	}
}
