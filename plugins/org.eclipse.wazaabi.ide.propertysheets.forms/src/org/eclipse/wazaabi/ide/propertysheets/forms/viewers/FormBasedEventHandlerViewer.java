package org.eclipse.wazaabi.ide.propertysheets.forms.viewers;

import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.EditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.forms.editinghelpers.FormBasedEditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.viewers.EventHandlerTableViewer;

public class FormBasedEventHandlerViewer extends EventHandlerTableViewer {
	protected EditingHelperFactory createEditingHelperFactory() {
		return new FormBasedEditingHelperFactory();
	}
}
