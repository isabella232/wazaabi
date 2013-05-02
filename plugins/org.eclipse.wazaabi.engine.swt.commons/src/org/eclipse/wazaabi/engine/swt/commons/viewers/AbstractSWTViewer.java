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

import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.viewers.AbstractEditPartViewer;
import org.eclipse.wazaabi.engine.core.viewers.AbstractWidgetRootEditPart;
import org.eclipse.wazaabi.engine.swt.commons.views.DeferredUpdateManager;
import org.eclipse.wazaabi.engine.swt.commons.views.UpdateManager;

public abstract class AbstractSWTViewer extends AbstractEditPartViewer {

	private UpdateManager manager = new DeferredUpdateManager();

	private final org.eclipse.swt.widgets.Composite parent;

	public AbstractSWTViewer(org.eclipse.swt.widgets.Composite parent) {
		this.parent = parent;
	}

	public UpdateManager getUpdateManager() {
		return manager;
	}

	protected void hookControl() {
		super.hookControl();
		if (getControl() == null)
			return;
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
		if (getControl() == null)
			return;
		super.unhookControl();
		// Ideally, you would want to remove the listeners here
		AbstractWidgetRootEditPart tep = (AbstractWidgetRootEditPart) getRootEditPart();
		tep.setWidgetView(null);
	}

	public org.eclipse.swt.widgets.Composite getParent() {
		return parent;
	}

	public abstract AbstractCompatibilityToolkit getAbstractCompatibilityToolkit();
}
