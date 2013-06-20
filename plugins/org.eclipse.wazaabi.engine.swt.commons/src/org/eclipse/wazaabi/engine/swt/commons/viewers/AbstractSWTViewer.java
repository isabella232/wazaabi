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

import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.viewers.AbstractEditPartViewer;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.swt.commons.impl.SWTRegistryImpl;
import org.eclipse.wazaabi.engine.swt.commons.views.DeferredUpdateManager;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;
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

	/**
	 * @see org.eclipse.wazaabi.engine.core.viewers.AbstractEditPartViewer#reveal(org.eclipse.wazaabi.engine.core.gef.EditPart)
	 */
	public void reveal(EditPart part) {
		// DOES NOTHING AT THE MOMENT
	}

	public org.eclipse.swt.widgets.Composite getParent() {
		return parent;
	}

	public abstract AbstractCompatibilityToolkit getAbstractCompatibilityToolkit();

	@Override
	protected void doSetContents(EditPart editpart) {
		super.doSetContents(editpart);
	}

	protected Widget getWidget() {
		if (!(getContents() instanceof AbstractWidgetEditPart))
			return null;
		if (((AbstractWidgetEditPart) getContents()).getWidgetView() instanceof SWTWidgetView) {
			return ((SWTWidgetView) ((AbstractWidgetEditPart) getContents())
					.getWidgetView()).getSWTWidget();
		}
		return null;
	}

	@Override
	protected Registry createRegistry() {
		return new SWTRegistryImpl();
	}

}
