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


public class BasicActionHandler {
	
	boolean disposed = true;

	public BasicActionHandler() {
		System.out.println("creating " + getClass().getName());
		disposed = false;
	}

	public int execute(int a, int b) {
		System.out.println("pushButton pressed");
		//System.out.println(dispatcher.getText());
		//dispatcher.setText("New");
		return a - b;
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
		disposed = true;
	}
	
	public boolean isDisposed() {
		return disposed;
	}
}
