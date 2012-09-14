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

package org.eclipse.wazaabi.engine.edp.tests.handlers;

import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;


public class BadConditionHandler {
	
	boolean disposed = true;

	public BadConditionHandler() {
		System.out.println("creating " + getClass().getName());
		disposed = false;
	}

	public boolean canExecute(int a, int b) {
		System.out.println("check condition ongoing");
		throw new OperationAborted("This can not execute");
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
		disposed = true;
	}
	
	public boolean isDisposed() {
		return disposed;
	}
}
