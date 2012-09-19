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

import org.eclipse.wazaabi.mm.core.extras.CellEditor;
import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;

public class CellEditorFactory {

	private static CellEditorFactory instance = null;

	protected CellEditorFactory() {
	}

	// TODO : this class is at the moment a singleton, it will be a OSGI
	// compliant factory in the future
	public static final CellEditorFactory getInstance() {
		if (instance == null)
			instance = new CellEditorFactory();
		return instance;
	}

	public org.eclipse.jface.viewers.CellEditor getCellEditor(
			CellEditor cellEditor) {
		if (cellEditor == null)
			return null;
		if (cellEditor.eClass() == CoreExtrasPackage.Literals.TEXT_CELL_EDITOR)
			return new org.eclipse.jface.viewers.TextCellEditor();

		return null;
	}

}
