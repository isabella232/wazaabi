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
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;

public class DynamicEditingSupport extends EditingSupport {

	private CellEditor cellEditor = null;
	private final ColumnDescriptor columnDescriptor;

	private AbstractCodeDescriptor.MethodDescriptor canEditMethodDescriptor = null;
	// TODO : very bad and verbose code
	// we should be able to get the codeDescriptor from the
	// methodDescriptor
	private AbstractCodeDescriptor canEditCodeDescriptor = null;

	private AbstractCodeDescriptor.MethodDescriptor getValueMethodDescriptor = null;
	private AbstractCodeDescriptor getValueCodeDescriptor = null;

	private AbstractCodeDescriptor.MethodDescriptor setValueMethodDescriptor = null;
	private AbstractCodeDescriptor setValueCodeDescriptor = null;

	private AbstractCodeDescriptor.MethodDescriptor getCellEditorMethodDescriptor = null;
	private AbstractCodeDescriptor getCellEditorCodeDescriptor = null;

	protected DynamicEditingSupport(ColumnViewer viewer,
			ColumnDescriptor columnDescriptor, CellEditor cellEditor) {
		super(viewer);
		this.cellEditor = cellEditor;
		this.columnDescriptor = columnDescriptor;
		update();
	}

	@Override
	protected CellEditor getCellEditor(Object element) {
		if (getCellEditorMethodDescriptor != null
				&& getCellEditorCodeDescriptor != null) {
			CellEditor _cellEditor = (CellEditor) getCellEditorCodeDescriptor
					.invokeMethod(getCellEditorMethodDescriptor, new Object[] {
							element, columnDescriptor });
			if (_cellEditor != null)
				_cellEditor
						.create((org.eclipse.swt.widgets.Composite) getViewer()
								.getControl());
			return _cellEditor;
		}
		return cellEditor;
	}

	@Override
	protected boolean canEdit(Object element) {
		if (canEditMethodDescriptor != null && canEditCodeDescriptor != null) {
			return (Boolean) canEditCodeDescriptor.invokeMethod(
					canEditMethodDescriptor, new Object[] { element,
							columnDescriptor });
		}
		return true;
	}

	@Override
	protected Object getValue(Object element) {
		if (getValueMethodDescriptor != null && getValueCodeDescriptor != null) {
			return getValueCodeDescriptor.invokeMethod(
					getValueMethodDescriptor, new Object[] { element,
							columnDescriptor });
		}
		return null;
	}

	@Override
	protected void setValue(Object element, Object value) {
		if (setValueMethodDescriptor != null && setValueCodeDescriptor != null) {
			setValueCodeDescriptor.invokeMethod(setValueMethodDescriptor,
					new Object[] { element, value, columnDescriptor });
			getViewer().update(element, null);
		}
	}

	public void update() {
		if (columnDescriptor != null
				&& columnDescriptor.getEditingSupport() != null
				&& !"".equals(columnDescriptor.getEditingSupport())) {
			AbstractCodeDescriptor codeDescriptor = EDPSingletons
					.getComposedCodeLocator().resolveCodeDescriptor(
							columnDescriptor.getEditingSupport());
			if (codeDescriptor != null) {
				AbstractCodeDescriptor.MethodDescriptor methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"canEdit", new String[] { "element", "columnDescriptor" }, new Class[] { Object.class, ColumnDescriptor.class }, Boolean.class); //$NON-NLS-1$  //$NON-NLS-2$
				if (methodDescriptor != null) {
					canEditMethodDescriptor = methodDescriptor;
					canEditCodeDescriptor = codeDescriptor;
				}
				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"getValue", new String[] { "element", "columnDescriptor" }, new Class[] { Object.class, ColumnDescriptor.class }, Object.class); //$NON-NLS-1$ //$NON-NLS-2$ 
				if (methodDescriptor != null) {
					getValueMethodDescriptor = methodDescriptor;
					getValueCodeDescriptor = codeDescriptor;
				}
				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"setValue", new String[] { "element", "value", "columnDescriptor" }, new Class[] { Object.class, Object.class, ColumnDescriptor.class }, null); //$NON-NLS-1$ //$NON-NLS-2$  //$NON-NLS-3$
				if (methodDescriptor != null) {
					setValueMethodDescriptor = methodDescriptor;
					setValueCodeDescriptor = codeDescriptor;
				}
				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"getCellEditor", new String[] { "element", "columnDescriptor" }, new Class[] { Object.class, ColumnDescriptor.class }, CellEditor.class); //$NON-NLS-1$ //$NON-NLS-2$  
				if (methodDescriptor != null) {
					getCellEditorMethodDescriptor = methodDescriptor;
					getCellEditorCodeDescriptor = codeDescriptor;
				}
			}
		}

	}

}
