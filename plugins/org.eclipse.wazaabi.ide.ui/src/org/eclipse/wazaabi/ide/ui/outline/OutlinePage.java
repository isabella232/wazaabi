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
import org.eclipse.gef.EditPartViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.part.Page;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.wazaabi.engine.swt.editparts.SWTRootEditPart;

public class OutlinePage extends Page implements IContentOutlinePage,
		IAdaptable, ISelectionChangedListener {

	private OutlineViewer wazaabiViewer;
	private final EditPartViewer editorPartViewer;
	private ISelection selection;
	private ArrayList<ISelectionChangedListener> listeners = new ArrayList<ISelectionChangedListener>();

	public OutlinePage(EditPartViewer editorPartViewer) {
		this.editorPartViewer = editorPartViewer;
	}

	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		return null;
	}

	@Override
	public void createControl(Composite parent) {
		createOutlineViewer(parent);
		if (getViewer() != null)
			getViewer().setContents(
					getEditorPartViewer().getContents().getModel());
		// TODO : we should detect whether the editorPartViewer's model changes
	}

	protected void createOutlineViewer(Composite parent) {
		wazaabiViewer = new OutlineViewer(parent, new SWTRootEditPart());
	}

	@Override
	public Control getControl() {
		if (getViewer() != null)
			return getViewer().getControl();
		return null;
	}

	@Override
	public void setFocus() {
		if (getViewer() != null && getViewer().getControl() != null
				&& !getViewer().getControl().isDisposed())
			getViewer().getControl().setFocus();
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

	public EditPartViewer getEditorPartViewer() {
		return editorPartViewer;
	}

	@Override
	public void dispose() {
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
