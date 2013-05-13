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

package org.eclipse.wazaabi.engine.swt.tests;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

public abstract class AbstractCommandTest {
	protected Display display = null;
	protected Shell mainShell = null;
	protected SWTControlViewer viewer = null;
	
	protected static final int INITIAL_WIDTH = 300;
	protected static final int INITIAL_HEIGHT = 300;
	
	
	protected final Display getDisplay() {
		return display;
	}
	
	protected final void setDisplay(Display display) {
		this.display = display;
	}
	
	protected final SWTControlViewer getViewer() {
		return viewer;
	}

	protected final Shell getMainShell() {
		return mainShell;
	}
	
	@Before
	public void before() {
		
		// init SWT Engine in standalone mode
		SWTHelper.init();
		
		// init the 'urn:java' resolver
		URNJavaLocatorHelper.init();
		
		// create the display
		Assert.assertNull(getDisplay());
		setDisplay(new Display());
		
		Assert.assertNotNull(display);
		Assert.assertTrue(!display.isDisposed());
		
		// create the shell which will receive the wazaabi components
		mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setText("Wazaabi");
		Assert.assertNotNull(mainShell);
		Assert.assertFalse(mainShell.isDisposed());
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		// create the viewer
		viewer = new SWTControlViewer(mainShell);
		Assert.assertNotNull(viewer);
	
	}
	
	@After
	public void after() {		
		Assert.assertNotNull(getDisplay());
		Assert.assertFalse(getDisplay().isDisposed());
		getDisplay().dispose();
		setDisplay(null);
	}
}
