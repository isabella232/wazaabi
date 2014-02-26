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
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
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
import org.eclipse.wazaabi.engine.swt.commons.editparts.SWTRootEditPart;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class AbstractOutlinePage extends Page implements
		IContentOutlinePage, IAdaptable, ISelectionChangedListener {

	final static Logger logger = LoggerFactory
			.getLogger(AbstractOutlinePage.class);

	private OutlineViewer outlineViewer;
	private ISelection selection;
	private ArrayList<ISelectionChangedListener> listeners = new ArrayList<ISelectionChangedListener>();
	private final EditPartViewer treeEditorViewer;
	private Composite container = null;

	public AbstractOutlinePage(EditPartViewer treeEditorViewer) {
		this.treeEditorViewer = treeEditorViewer;
	}

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		listeners.add(listener);
	}

	@Override
	public void createControl(Composite parent) {
		container = new Composite(parent, SWT.NONE);
		container.setLayout(new FillLayout());
		createOutlineViewer(container);
		initializeOutlineViewer();
	}

	protected void createOutlineViewer(Composite parent) {
		outlineViewer = new OutlineViewer(parent, new SWTRootEditPart());
	}

	@Override
	public void dispose() {
		listeners.clear();
		super.dispose();
	}

	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		return null;
	}

	@Override
	public Control getControl() {
		return container;
	}

	protected Object getEditorModel() {
		if (getTreeEditorViewer().getContents() != null)
			return getTreeEditorViewer().getContents().getModel();
		return null;
	}

	public OutlineViewer getOutlineViewer() {
		return outlineViewer;
	}

	public ISelection getSelection() {
		return selection;
	}

	public EditPartViewer getTreeEditorViewer() {
		return treeEditorViewer;
	}

	protected Object getVisibleContents(Object contents) {
		if (contents instanceof Resource
				&& !((Resource) contents).getContents().isEmpty())
			return ((Resource) contents).getContents().get(0);
		return contents;
	}

	protected void initializeOutlineViewer() {
		setViewerContents(getVisibleContents(getEditorModel()));
	}

	public void refreshSelection() {
		if (getOutlineViewer() != null
				&& getOutlineViewer().getControl() != null
				&& !getOutlineViewer().getControl().isDisposed())
			getOutlineViewer().refreshSelection();
	}

	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		listeners.remove(listener);
	}

	public void selectionChanged(SelectionChangedEvent event) {
		// we notify all the selectionChangedListeners of this viewer
		setSelection(event.getSelection());
	}

	@Override
	public void setFocus() {
		if (container != null && !container.isDisposed())
			container.setFocus();
	}

	public void setSelection(ISelection selection) {
		if (selection == null) {
			if (this.selection == null)
				return;
		} else if (selection.equals(this.selection))
			return;

		SelectionChangedEvent e = new SelectionChangedEvent(this, selection);
		if (getOutlineViewer() != null)
			getOutlineViewer().selectionChanged(e);
		this.selection = selection;
		if (listeners == null)
			return;
		for (int i = 0; i < listeners.size(); i++) {
			((ISelectionChangedListener) listeners.get(i)).selectionChanged(e);
		}
	}

	protected void setViewerContents(Object newContents) {
		if (newContents instanceof EObject) {
			getOutlineViewer().setContents(copy((EObject) newContents));
		} else
			getOutlineViewer().setContents(newContents);
		if (newContents instanceof Notifier)
			container.layout(true, true);
	}

	public static <T extends EObject> T copy(T eObject) {
		SpecialCopier copier = new SpecialCopier();
		EObject result = copier.copy(eObject);
		copier.copyReferences();

		@SuppressWarnings("unchecked")
		T t = (T) result;
		return t;
	}

	protected static class SpecialCopier extends EcoreUtil.Copier {

		private static final long serialVersionUID = 1L;

		protected EObject createCopy(EObject eObject) {
			if (eObject instanceof PlaceHolderRule) {
				return new PlaceHolderRule(
						((PlaceHolderRule) eObject).getStyleRuleDescriptor());
			}
			return super.createCopy(eObject);
		}

	};
}
