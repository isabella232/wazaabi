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

package org.eclipse.wazaabi.ide.ui.outline;

import java.util.ArrayList;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.part.Page;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.wazaabi.engine.swt.editparts.SWTRootEditPart;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OutlinePage extends Page implements IContentOutlinePage,
		IAdaptable, ISelectionChangedListener {

	final static Logger logger = LoggerFactory.getLogger(OutlinePage.class);
	private OutlineViewer wazaabiViewer;
	private final EditPartViewer editorEditPartViewer;
	private ISelection selection;
	private ArrayList<ISelectionChangedListener> listeners = new ArrayList<ISelectionChangedListener>();

	private Adapter resourceContentsTracker = new AdapterImpl() {

		@Override
		public void notifyChanged(Notification msg) {
			if (getViewer() != null)
				switch (msg.getEventType()) {
				case Notification.SET:
				case Notification.ADD:
					getViewer().setContents(msg.getNewValue());
					break;
				case Notification.REMOVE:
					getViewer().setContents(null);
					break;
				case Notification.ADD_MANY:
				case Notification.REMOVE_MANY:
					logger.error("Add_MANY and/or REMOVE_MANY not allowed here");
				}
		}

	};

	public OutlinePage(EditPartViewer editorEditPartViewer) {
		this.editorEditPartViewer = editorEditPartViewer;
	}

	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		return null;
	}

	@Override
	public void createControl(Composite parent) {
		createOutlineViewer(parent);
		initializeOutlineViewer();
	}

	protected void initializeOutlineViewer() {
		if (getEditorEditPartViewer().getContents() != null) {
			Object contents = getEditorEditPartViewer().getContents()
					.getModel();
			if (contents instanceof Resource)
				((Resource) contents).eAdapters().add(resourceContentsTracker);
			if (getViewer() != null)
				getViewer().setContents(getVisibleContents(contents));
		}
	}

	protected Object getVisibleContents(Object contents) {
		if (contents instanceof Resource
				&& !((Resource) contents).getContents().isEmpty())
			return ((Resource) contents).getContents().get(0);
		return contents;
	}

	private Composite container = null;

	protected void createOutlineViewer(Composite parent) {
		container = new Composite(parent, SWT.NONE);
		container.setLayout(new FillLayout());
		wazaabiViewer = new OutlineViewer(container, new SWTRootEditPart());
	}

	@Override
	public Control getControl() {
		return container;
	}

	@Override
	public void setFocus() {
		if (container != null && !container.isDisposed())
			container.setFocus();
	}

	public OutlineViewer getViewer() {
		return wazaabiViewer;
	}

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		listeners.add(listener);
	}

	public ISelection getSelection() {
		return selection;
	}

	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		listeners.remove(listener);
	}

	public void setSelection(ISelection selection) {
		if (selection == null) {
			if (this.selection == null)
				return;
		} else if (selection.equals(this.selection))
			return;

		SelectionChangedEvent e = new SelectionChangedEvent(this, selection);
		if (getViewer() != null)
			getViewer().selectionChanged(e);
		this.selection = selection;
		if (listeners == null)
			return;
		for (int i = 0; i < listeners.size(); i++) {
			((ISelectionChangedListener) listeners.get(i)).selectionChanged(e);
		}
	}

	public EditPartViewer getEditorEditPartViewer() {
		return editorEditPartViewer;
	}

	@Override
	public void dispose() {
		if (getEditorEditPartViewer().getContents() != null) {
			Object contents = getEditorEditPartViewer().getContents()
					.getModel();
			if (contents instanceof Resource)
				((Resource) contents).eAdapters().remove(
						resourceContentsTracker);
		}
		listeners.clear();
		super.dispose();
	}

	public void selectionChanged(SelectionChangedEvent event) {
		// we notify all the selectionChangedListeners of this viewer
		setSelection(event.getSelection());
	}

	public void refreshSelection() {
		if (getViewer() != null && getViewer().getControl() != null
				&& !getViewer().getControl().isDisposed())
			getViewer().refreshSelection();
	}

}
