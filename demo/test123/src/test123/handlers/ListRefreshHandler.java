package test123.handlers;

import org.eclipse.wazaabi.demo.contacts.database.DataService;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ListRefreshHandler {

	public void execute(Collection collection, EventHandler eventHandler,
			Event event) {
		collection.setInput(new DataService().getDatabase());
	}
}