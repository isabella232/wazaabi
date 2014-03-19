package test123.handlers;

import org.eclipse.e4.ui.workbench.modeling.ESelectionService;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ListSelectionHandler {

	public void execute(Collection collection, EventHandler eventHandler,
			Event event) {
		ESelectionService selectionService = (ESelectionService) collection
				.get("SelectionService");
		if (selectionService != null)
			selectionService.setSelection(collection.getSelection());
	}
}