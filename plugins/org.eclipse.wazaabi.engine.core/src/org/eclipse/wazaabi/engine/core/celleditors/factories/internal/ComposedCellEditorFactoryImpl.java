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

package org.eclipse.wazaabi.engine.core.celleditors.factories.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.core.celleditors.factories.CellEditorFactory;
import org.eclipse.wazaabi.engine.core.celleditors.factories.ComposedCellEditorFactory;
import org.eclipse.wazaabi.mm.core.extras.CellEditor;

public class ComposedCellEditorFactoryImpl implements ComposedCellEditorFactory {

	private List<CellEditorFactory> cellEditorFactories = new ArrayList<CellEditorFactory>();

	public Object createCellEditor(CellEditor cellEditor, Object creationHint) {
		CellEditorFactory cellEditorFactory = getCellEditorFactoryFor(cellEditor);
		if (cellEditorFactory != null)
			return cellEditorFactory.createCellEditor(cellEditor, creationHint);
		return null;
	}

	public boolean isFactoryFor(CellEditor cellEditor) {
		for (CellEditorFactory cellEditorFactory : cellEditorFactories)
			if (cellEditorFactory.isFactoryFor(cellEditor))
				return true;
		return false;
	}

	protected CellEditorFactory getCellEditorFactoryFor(CellEditor cellEditor) {
		for (CellEditorFactory cellEditorFactory : cellEditorFactories)
			if (cellEditorFactory.isFactoryFor(cellEditor))
				return cellEditorFactory;
		return null;
	}

	public void addCellEditorFactory(CellEditorFactory cellEditorFactory) {
		if (!cellEditorFactories.contains(cellEditorFactory))
			cellEditorFactories.add(cellEditorFactory);

	}

	public void removeCellEditorFactory(CellEditorFactory cellEditorFactory) {
		cellEditorFactories.remove(cellEditorFactory);
	}

}
