package org.eclipse.wazaabi.ide.launchconfiguration;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.commands.ITerminateHandler;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
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

import org.eclipse.debug.core.model.IThread;

public class WazaabiDebugTarget implements IDebugTarget {

	public String getModelIdentifier() {
		// TODO Auto-generated method stub
		return null;
	}

	public IDebugTarget getDebugTarget() {
		// TODO Auto-generated method stub
		return null;
	}

	public ILaunch getLaunch() {
		// TODO Auto-generated method stub
		return null;
	}

	public Object getAdapter(Class adapter) {
		// TODO Auto-generated method stub
		ITerminateHandler o=null;
		return null;
	}

	public boolean canTerminate() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isTerminated() {
		// TODO Auto-generated method stub
		return false;
	}

	public void terminate() throws DebugException {
		System.out.println("terminate");

	}

	public boolean canResume() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canSuspend() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isSuspended() {
		// TODO Auto-generated method stub
		return false;
	}

	public void resume() throws DebugException {
		System.out.println("resume()");

	}

	public void suspend() throws DebugException {
		System.out.println("supend()");

	}

	public void breakpointAdded(IBreakpoint breakpoint) {
		// TODO Auto-generated method stub

	}

	public void breakpointRemoved(IBreakpoint breakpoint, IMarkerDelta delta) {
		// TODO Auto-generated method stub

	}

	public void breakpointChanged(IBreakpoint breakpoint, IMarkerDelta delta) {
		// TODO Auto-generated method stub

	}

	public boolean canDisconnect() {
		// TODO Auto-generated method stub
		return false;
	}

	public void disconnect() throws DebugException {
		// TODO Auto-generated method stub

	}

	public boolean isDisconnected() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean supportsStorageRetrieval() {
		// TODO Auto-generated method stub
		return false;
	}

	public IMemoryBlock getMemoryBlock(long startAddress, long length)
			throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public IProcess getProcess() {
		// TODO Auto-generated method stub
		return null;
	}

	public IThread[] getThreads() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean hasThreads() throws DebugException {
		// TODO Auto-generated method stub
		return false;
	}

	public String getName() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean supportsBreakpoint(IBreakpoint breakpoint) {
		// TODO Auto-generated method stub
		return false;
	}

}
