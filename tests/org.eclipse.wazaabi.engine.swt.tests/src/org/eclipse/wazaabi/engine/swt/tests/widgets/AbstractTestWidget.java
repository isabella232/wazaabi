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

package org.eclipse.wazaabi.engine.swt.tests.widgets;


import org.eclipse.wazaabi.engine.swt.tests.AbstractCommandTest;

public abstract class AbstractTestWidget extends AbstractCommandTest {
	
	
	
	@Override
	public void before() {
		super.before();
			
	}
	
	@Override
	public void after() {
		mainShell.open();
		
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
		
		super.after();
	}
	
}
