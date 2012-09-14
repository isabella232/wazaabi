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

package org.eclipse.wazaabi.engine.swt.viewers;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.wazaabi.engine.core.gef.RootEditPart;
import org.eclipse.wazaabi.engine.swt.editparts.SWTRootEditPart;

/**
 * 
 * 
 * @author Olivier Moises
 * 
 */
public class SWTShellViewer extends SWTControlViewer {

	private boolean souldDeleteDisplayOnDispose = false;

	private org.eclipse.swt.widgets.Display display = null;

	public SWTShellViewer() {
		super(null);
		this.display = new org.eclipse.swt.widgets.Display();
		souldDeleteDisplayOnDispose = true;
	}

	public SWTShellViewer(org.eclipse.swt.widgets.Display display) {
		super(null);
		this.display = display;
	}

//	public SWTShellViewer(org.eclipse.swt.widgets.Display display,
//			org.eclipse.wazaabi.mm.swt.widgets.Window window) {
//		this(display);
//		setContents(window);
//	}
//
//	public SWTShellViewer(org.eclipse.swt.widgets.Shell parent,
//			org.eclipse.wazaabi.mm.swt.widgets.Window window) {
//		super(parent);
//		this.display = parent.getDisplay();
//		setContents(window);
//	}
//
//	public SWTShellViewer(org.eclipse.swt.widgets.Display display, String uri) {
//		this(display,
//				(org.eclipse.wazaabi.mm.swt.widgets.Window) getModelComponent(uri));
//	}
//
//	public SWTShellViewer(org.eclipse.wazaabi.mm.swt.widgets.Window window) {
//		this();
//		setContents(window);
//	}

//	public SWTShellViewer(String uri) {
//		this((org.eclipse.wazaabi.mm.swt.widgets.Window) getModelComponent(uri));
//	}

	public org.eclipse.swt.widgets.Display getDisplay() {
		return this.display;
	}

	protected org.eclipse.swt.widgets.Shell getShell() {
		return (org.eclipse.swt.widgets.Shell) getControl();
	}

	public void handleDispose(DisposeEvent e) {
		super.handleDispose(e);
		if (souldDeleteDisplayOnDispose && this.display != null
				&& !this.display.isDisposed())
			this.display.dispose();
		System.out.println("ShellViewer disposed");
	}

	public boolean isDisposed() {
		if (getShell() != null)
			return getShell().isDisposed();
		return true;
	}

	public void loop() {
		while (!getShell().isDisposed()) {
			if (!getShell().getDisplay().readAndDispatch())
				getShell().getDisplay().sleep();
		}
	}

	public void open() {
		getShell().open();
	}

	public void setRootEditPart(RootEditPart editpart) {
		assert editpart == null || editpart instanceof SWTRootEditPart;
		super.setRootEditPart(editpart);
	}

	protected void addDisposeListener() {
		// the disposeListener is attached to the Shell itself
		getControl().addDisposeListener(getDisposeListener());
	}
}
