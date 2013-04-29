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

package org.eclipse.wazaabi.engine.swt.celleditors.factories;

import org.eclipse.wazaabi.engine.core.celleditors.factories.CellEditorFactory;
import org.eclipse.wazaabi.mm.core.extras.CellEditor;
import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;

public class SWTCellEditorFactory implements CellEditorFactory {

	public Object createCellEditor(CellEditor cellEditor, Object creationHint) {
		if (cellEditor == null)
			return null;
		if (cellEditor.eClass() == CoreExtrasPackage.Literals.TEXT_CELL_EDITOR)
			return new org.eclipse.jface.viewers.TextCellEditor();
		if (cellEditor.eClass() == CoreExtrasPackage.Literals.CHECKBOX_CELL_EDITOR)
			return new org.eclipse.jface.viewers.CheckboxCellEditor();
		return null;
	}

	public boolean isFactoryFor(CellEditor cellEditor) {
		if (cellEditor != null)
			return cellEditor.eClass() == CoreExtrasPackage.Literals.TEXT_CELL_EDITOR
					|| cellEditor.eClass() == CoreExtrasPackage.Literals.CHECKBOX_CELL_EDITOR;
		return false;
	}

}
