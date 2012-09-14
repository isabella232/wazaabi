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

public class DynamicEditingSupport extends EditingSupport {

	private CellEditor cellEditor = null;

	protected DynamicEditingSupport(ColumnViewer viewer, CellEditor cellEditor) {
		super(viewer);
		this.cellEditor = cellEditor;
	}

	@Override
	protected CellEditor getCellEditor(Object element) {
		return cellEditor;
	}

	@Override
	protected boolean canEdit(Object element) {
		return true;
	}

	@Override
	protected Object getValue(Object element) {
		System.out.println(element);
		return 12;
	}

	@Override
	protected void setValue(Object element, Object value) {
		System.out.println(value);

	}


}
