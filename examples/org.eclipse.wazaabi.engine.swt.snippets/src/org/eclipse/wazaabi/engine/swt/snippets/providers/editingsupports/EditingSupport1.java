package org.eclipse.wazaabi.engine.swt.snippets.providers.editingsupports;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;

public class EditingSupport1 {

	// if not present, returns always true
	// column Descriptor is optional
	public boolean canEdit(Object element, ColumnDescriptor columnDescriptor) {
		return false;
	}

	// optional, if not present, returns the cell editor attached to this column
	public CellEditor getCellEditor(Object element) {
		return null;
	}

	// returns the value passed to the cell editor for a given element
	public Object getValue(Object element) {
		return null;
	}

	//
	public void setValue(Object element, Object value) {
	}

}
