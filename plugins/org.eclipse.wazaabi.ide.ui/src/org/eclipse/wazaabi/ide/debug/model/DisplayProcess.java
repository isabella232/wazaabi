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
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;

public class DisplayProcess implements IProcess {

	private final ILaunch launch;

	public DisplayProcess(ILaunch launch) {
		this.launch = launch;
	}

	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		System.out.println("getAdapter(" + adapter + ")");
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

	public String getLabel() {
		return "kiki";
	}

	public ILaunch getLaunch() {
		return launch;
	}

	public IStreamsProxy getStreamsProxy() {
		return null;
	}

	public void setAttribute(String key, String value) {
		System.out.println("setAttribute: " + key + ",value=" + value);

	}

	public String getAttribute(String key) {
		System.out.println("getAttribute (" + key + ")");
		return null;
	}

	public int getExitValue() throws DebugException {
		return 0;
	}

}
