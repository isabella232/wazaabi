package org.eclipse.wazaabi.engine.swt.addressbook.handlers;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class MasterToDetailHandler {

	public void execute(EventDispatcher dispatcher, EventHandler handler, Event event) {
		if (dispatcher instanceof Collection) {
			EObject selection = (EObject) ((Collection) dispatcher).getSelection();
			System.out.println(selection);
			
			// Here we should clone Model and inject in detail part
		} else {
			System.out.println("other");
		}
	}
}
