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

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;

public class ViewerThread implements IThread {

	private final IDebugTarget debugTarget;
	private final ILaunch launch;

	private boolean isTerminated = false;
	private boolean isResumed = false;

	public ViewerThread(ILaunch launch, IDebugTarget debugTarget) {
		this.debugTarget = debugTarget;
		this.launch = launch;
	}

	public String getModelIdentifier() {
		return "modelIdentifier";
	}

	public IDebugTarget getDebugTarget() {
		return debugTarget;
	}

	public ILaunch getLaunch() {
		return launch;
	}

	public Object getAdapter(Class adapter) {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean canResume() {
		return isResumed == false;
	}

	public boolean canSuspend() {
		return !canResume();
	}

	public boolean isSuspended() {
		return !isResumed;
	}

	public void resume() throws DebugException {
		isResumed = true;
	}

	public void suspend() throws DebugException {
		isResumed = false;
	}

	public boolean canStepInto() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canStepOver() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean canStepReturn() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean isStepping() {
		// TODO Auto-generated method stub
		return false;
	}

	public void stepInto() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void stepOver() throws DebugException {
		// TODO Auto-generated method stub

	}

	public void stepReturn() throws DebugException {
		// TODO Auto-generated method stub

	}

	public boolean canTerminate() {
		return isTerminated = false;
	}

	public boolean isTerminated() {
		return isTerminated;
	}

	public void terminate() throws DebugException {
		System.out.println("terminate");
		isTerminated = true;
	}

	public IStackFrame[] getStackFrames() throws DebugException {
		return new IStackFrame[] {};
	}

	public boolean hasStackFrames() throws DebugException {
		// TODO Auto-generated method stub
		return false;
	}

	public int getPriority() throws DebugException {
		// TODO Auto-generated method stub
		return 0;
	}

	public IStackFrame getTopStackFrame() throws DebugException {
		// TODO Auto-generated method stub
		return null;
	}

	public String getName() throws DebugException {
		return "thread waz";
	}

	public IBreakpoint[] getBreakpoints() {
		// TODO Auto-generated method stub
		return null;
	}

}
