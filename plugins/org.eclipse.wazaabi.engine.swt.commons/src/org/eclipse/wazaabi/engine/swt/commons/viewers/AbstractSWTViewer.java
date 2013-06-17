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

package org.eclipse.wazaabi.engine.swt.commons.viewers;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.TypedListener;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.viewers.AbstractEditPartViewer;
import org.eclipse.wazaabi.engine.swt.commons.views.DeferredUpdateManager;
import org.eclipse.wazaabi.engine.swt.commons.views.UpdateManager;

public abstract class AbstractSWTViewer extends AbstractEditPartViewer {

	private UpdateManager manager = new DeferredUpdateManager();

	private final org.eclipse.swt.widgets.Composite parent;

	private DisposeListener disposeListener = new DisposeListener() {

		@Override
		public void widgetDisposed(DisposeEvent e) {
			AbstractSWTViewer.this.dispose();
		}
	};

	public AbstractSWTViewer(org.eclipse.swt.widgets.Composite parent) {
		this.parent = parent;
	}

	public UpdateManager getUpdateManager() {
		return manager;
	}

	protected void hookControl() {
		super.hookControl();
		if (getControl() instanceof Control)
			addUniqueDisposeListener((Control) getControl());
	}

	/**
	 * @see org.eclipse.wazaabi.engine.core.viewers.AbstractEditPartViewer#reveal(org.eclipse.wazaabi.engine.core.gef.EditPart)
	 */
	public void reveal(EditPart part) {
		// DOES NOTHING AT THE MOMENT
	}

	/**
	 * Unhooks a control so that it can be reset. This method deactivates the
	 * contents, removes the Control as being the Control of the
	 * RootTreeEditPart, etc. It does not remove the listeners because it is
	 * causing errors, although that would be a desirable outcome.
	 */
	protected void unhookControl() {
		if (getControl() instanceof Control
				&& !((Control) getControl()).isDisposed())
			((Control) getControl()).removeDisposeListener(disposeListener);
		super.unhookControl();
	}

	public org.eclipse.swt.widgets.Composite getParent() {
		return parent;
	}

	public abstract AbstractCompatibilityToolkit getAbstractCompatibilityToolkit();

	@Override
	protected void doSetContents(EditPart editpart) {
		super.doSetContents(editpart);
		if (getControl() instanceof Control)
			addUniqueDisposeListener((Control) getControl());
	}

	/**
	 * Adds a {@link DisposeListener} to this control. Ensures the dispose
	 * listener is not already present before to add it.
	 * 
	 * @param control
	 *            a non null {@link Control}
	 */
	protected void addUniqueDisposeListener(Control control) {
		if (control.isDisposed())
			return;
		boolean found = false;
		for (Listener listener : control.getListeners(SWT.Dispose))
			if (listener instanceof TypedListener
					&& ((TypedListener) listener).getEventListener() == disposeListener)
				return;
		if (!found)
			control.addDisposeListener(disposeListener);
	}
}
