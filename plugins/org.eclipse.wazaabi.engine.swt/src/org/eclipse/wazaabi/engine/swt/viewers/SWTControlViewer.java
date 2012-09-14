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

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.gef.RootEditPart;
import org.eclipse.wazaabi.engine.swt.editparts.SWTRootEditPart;
import org.eclipse.wazaabi.engine.swt.views.SWTWidgetView;

/**
 * 
 * 
 * @author Olivier Moises
 * 
 */
public class SWTControlViewer extends AbstractSWTViewer {

	// PROPOSAL : not sure to keep it
	protected static org.eclipse.wazaabi.mm.core.widgets.AbstractComponent getModelComponent(
			String uri) {
		Resource resource = new ResourceSetImpl().getResource(
				URI.createURI(uri), true);
		if (resource.getEObject("/") instanceof org.eclipse.wazaabi.mm.core.widgets.AbstractComponent)
			return (org.eclipse.wazaabi.mm.core.widgets.AbstractComponent) resource
					.getEObject("/");
		return null;
	}

	public SWTControlViewer(org.eclipse.swt.widgets.Composite parent,
			SWTRootEditPart rootEditPart) {
		super(parent);
		setRootEditPart(rootEditPart);
	}

	public SWTControlViewer(org.eclipse.swt.widgets.Composite parent) {
		this(parent, new SWTRootEditPart());
		// setRootEditPart(new SWTRootEditPart());
		// setEditDomain(new EditDomain());

		// EditDomain editDomain = new EditDomain();
		// editDomain.setActiveTool(new SelectionTool());
	}

//	public SWTControlViewer(org.eclipse.swt.widgets.Composite parent,
//			org.eclipse.wazaabi.mm.swt.widgets.SWTControl window) {
//		this(parent);
//		setContents(window);
//	}
//
//	public SWTControlViewer(org.eclipse.swt.widgets.Composite parent, String uri) {
//		this(
//				parent,
//				(org.eclipse.wazaabi.mm.swt.widgets.SWTControl) getModelComponent(uri));
//	}

	public void setContents(Object contents) {
		assert getEditPartFactory() != null;
		setContents(getEditPartFactory().createEditPart(getRootEditPart(),
				contents));
		addDisposeListener();
	}

	private DisposeListener disposeListener = new DisposeListener() {
		public void widgetDisposed(DisposeEvent e) {
			SWTControlViewer.this.handleDispose(e);
		}
	};

	private boolean hasDisposeListener = false;

	protected void addDisposeListener() {
		// we must be sure to not add more than one time the same
		// disposeListener

		if (!hasDisposeListener) {
			getParent().addDisposeListener(getDisposeListener());
			hasDisposeListener = true;
		}
	}

	public Control getControl() {
		if (!(getContents() instanceof AbstractWidgetEditPart))
			return null;
		if (((AbstractWidgetEditPart) getContents()).getWidgetView() instanceof SWTWidgetView) {
			return (Control) ((SWTWidgetView) ((AbstractWidgetEditPart) getContents())
					.getWidgetView()).getSWTWidget();
		}
		return null;
	}

	public EditPart getContents() {
		if (getRootEditPart() == null)
			return null;
		return getRootEditPart().getContents();
	}

	public void handleDispose(DisposeEvent e) {
		// super.handleDispose(e);
		System.out.println("Viewer disposed");
	}

	public void setRootEditPart(RootEditPart editpart) {
		assert editpart == null || editpart instanceof SWTRootEditPart;
		super.setRootEditPart(editpart);
		if (!getRootEditPart().isActive())
			getRootEditPart().activate();
	}

	protected final DisposeListener getDisposeListener() {
		return disposeListener;
	}
}
