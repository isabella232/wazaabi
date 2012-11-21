package org.eclipse.wazaabi.engine.swt.addressbook.handlers;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.wazaabi.engine.swt.addressbook.AddressBookUIHelper;
import org.eclipse.wazaabi.engine.swt.model.addressbook.Person;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class MasterToDetailHandler {

	public void execute(EventDispatcher dispatcher, EventHandler handler, Event event) {
		if (dispatcher instanceof Collection) {
			if (!((Collection) dispatcher).getSelection().isEmpty()) {
				System.out.println(((Collection) dispatcher).getSelection().get(0));
				Person person = (Person) ((Collection) dispatcher).getSelection().get(0);
				
				EObject model = EcoreUtil.copy(person);
				
				Container root = ((Container)dispatcher.eContainer().eContainer());
				if (!root.getElementsById("detailContainer").isEmpty()){
					Container detailCont = (Container) root.getElementsById("detailContainer").get(0);
					Container personDetail = AddressBookUIHelper.createPersonDetailUI();
					personDetail.set("detailmodel", model);
					
					root.getChildren().remove(detailCont);
					root.getChildren().add(personDetail);
				}
			}
			
			// Here we should clone Model and inject in detail part
		} else {
			System.out.println("other");
		}
	}
}
