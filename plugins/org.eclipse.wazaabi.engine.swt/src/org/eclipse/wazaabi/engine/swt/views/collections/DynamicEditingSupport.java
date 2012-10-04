/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.views.collections;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;

public class DynamicEditingSupport extends EditingSupport {

	private CellEditor cellEditor = null;
	private final AbstractColumnDescriptor columnDescriptor;
	private final ColumnManager columnManager;

	private AbstractCodeDescriptor.MethodDescriptor canEditMethodDescriptor = null;
	// TODO : very bad and verbose code
	// we should be able to get the codeDescriptor from the
	// methodDescriptor
	private AbstractCodeDescriptor editingSupportCodeDescriptor = null;

	private AbstractCodeDescriptor.MethodDescriptor getValueMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor setValueMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getCellEditorMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor disposeMethodDescriptor = null;

	protected DynamicEditingSupport(ColumnManager columnManager,
			AbstractColumnDescriptor columnDescriptor) {
		super((ColumnViewer) columnManager.getCollectionView().getViewer());
		this.columnManager = columnManager;
		this.columnDescriptor = columnDescriptor;
		update();
	}

	@Override
	protected CellEditor getCellEditor(Object element) {
		if (cellEditor != null)
			return cellEditor;
		// is the cell editor defined in the code ?
		if (getCellEditorMethodDescriptor != null
				&& editingSupportCodeDescriptor != null) {
			cellEditor = (CellEditor) editingSupportCodeDescriptor
					.invokeMethod(getCellEditorMethodDescriptor, new Object[] {
							element, columnDescriptor });
			if (cellEditor != null && cellEditor.getControl() == null)
				cellEditor
						.create((org.eclipse.swt.widgets.Composite) getViewer()
								.getControl());
		} // is the cell editor defined in the model?
		else if (columnDescriptor.getCellEditor() != null)
			cellEditor = columnManager.getModelCellEditor(columnDescriptor
					.getCellEditor());

		// otherwise the cell editor is a jFace TextCellEditor
		if (cellEditor == null)
			cellEditor = new org.eclipse.jface.viewers.TextCellEditor(
					(org.eclipse.swt.widgets.Composite) getViewer()
							.getControl());
		return cellEditor;
	}

	@Override
	protected boolean canEdit(Object element) {
		if (canEditMethodDescriptor != null
				&& editingSupportCodeDescriptor != null) {
			return (Boolean) editingSupportCodeDescriptor.invokeMethod(
					canEditMethodDescriptor, new Object[] { element,
							columnDescriptor });
		}
		return true;
	}

	@Override
	protected Object getValue(Object element) {
		if (getValueMethodDescriptor != null
				&& editingSupportCodeDescriptor != null) {
			return editingSupportCodeDescriptor.invokeMethod(
					getValueMethodDescriptor, new Object[] { element,
							columnDescriptor });
		}
		return null;
	}

	@Override
	protected void setValue(Object element, Object value) {
		if (setValueMethodDescriptor != null
				&& editingSupportCodeDescriptor != null) {
			editingSupportCodeDescriptor.invokeMethod(setValueMethodDescriptor,
					new Object[] { element, value, columnDescriptor });
			getViewer().update(element, null);
		}
	}

	public void update() {
		if (columnDescriptor != null
				&& columnDescriptor.getEditingSupport() != null
				&& !"".equals(columnDescriptor.getEditingSupport())) {
			editingSupportCodeDescriptor = EDPSingletons
					.getComposedCodeLocator().resolveCodeDescriptor(
							columnDescriptor.getEditingSupport());
			if (editingSupportCodeDescriptor != null) {
				AbstractCodeDescriptor.MethodDescriptor methodDescriptor = editingSupportCodeDescriptor
						.getMethodDescriptor(
								"canEdit", new String[] { "element", "columnDescriptor" }, new Class[] { Object.class, ColumnDescriptor.class }, Boolean.class); //$NON-NLS-1$  //$NON-NLS-2$
				if (methodDescriptor != null)
					canEditMethodDescriptor = methodDescriptor;

				methodDescriptor = editingSupportCodeDescriptor
						.getMethodDescriptor(
								"getValue", new String[] { "element", "columnDescriptor" }, new Class[] { Object.class, ColumnDescriptor.class }, Object.class); //$NON-NLS-1$ //$NON-NLS-2$ 
				if (methodDescriptor != null)
					getValueMethodDescriptor = methodDescriptor;

				methodDescriptor = editingSupportCodeDescriptor
						.getMethodDescriptor(
								"setValue", new String[] { "element", "value", "columnDescriptor" }, new Class[] { Object.class, Object.class, ColumnDescriptor.class }, null); //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$
				if (methodDescriptor != null)
					setValueMethodDescriptor = methodDescriptor;

				methodDescriptor = editingSupportCodeDescriptor
						.getMethodDescriptor(
								"getCellEditor", new String[] { "element", "columnDescriptor" }, new Class[] { Object.class, ColumnDescriptor.class }, CellEditor.class); //$NON-NLS-1$ //$NON-NLS-2$  
				if (methodDescriptor != null)
					getCellEditorMethodDescriptor = methodDescriptor;

				methodDescriptor = editingSupportCodeDescriptor
						.getMethodDescriptor("dispose", null, null, null); //$NON-NLS-1$ //$NON-NLS-2$  
				if (methodDescriptor != null)
					disposeMethodDescriptor = methodDescriptor;
			}
		}

	}

	public void dispose() {
		// call the EditingSupport dispose() if present
		if (disposeMethodDescriptor != null
				&& editingSupportCodeDescriptor != null) {
			editingSupportCodeDescriptor.invokeMethod(disposeMethodDescriptor,
					null);
		}
	}
}
