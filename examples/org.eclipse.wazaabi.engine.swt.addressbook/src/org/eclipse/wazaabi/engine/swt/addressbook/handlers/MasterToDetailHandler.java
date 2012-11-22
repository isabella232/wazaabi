package org.eclipse.wazaabi.engine.swt.addressbook.handlers;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.wazaabi.engine.core.CoreUtils;
import org.eclipse.wazaabi.engine.swt.addressbook.AddressBookUIHelper;
import org.eclipse.wazaabi.engine.swt.model.addressbook.Address;
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
				EObject selection = (EObject) ((Collection) dispatcher).getSelection().get(0);
				Container root = ((Container)dispatcher.eContainer().eContainer());
				
				checkAndSaveDirty((EObject)root.get("currentobject"),(EObject)root.get("detailmodel"));
				
				EObject model = EcoreUtil.copy(selection);
				root.set("detailmodel", model);
				root.set("currentobject",selection);

				if (!root.getElementsById("detailContainer").isEmpty()){
					Container detailContainer = (Container) root.getElementsById("detailContainer").get(0);

					if (selection instanceof Person) {
						Container personForm = AddressBookUIHelper.createPersonDetailUI();
						
						detailContainer.getChildren().clear();
						detailContainer.getChildren().add(personForm);
					} else if (selection instanceof Address) {
						Container addressForm = AddressBookUIHelper.createAddressDetailUI();
						
						detailContainer.getChildren().clear();
						detailContainer.getChildren().add(addressForm);
					} else {
						detailContainer.getChildren().clear();
					}
				}
				
				Collection col = (Collection) ((Container)root.getChildren().get(0)).getChildren().get(0);
				CoreUtils.refreshContent(col);
				col.getSelection().add(selection);
			}
			
		} else {
			System.out.println("other");
		}
	}

	private void checkAndSaveDirty(EObject model, EObject edited) {
		if (edited != null) {
			for (EStructuralFeature feature : edited.eClass().getEAllStructuralFeatures()) {
				if (feature instanceof EAttribute) {
					Object editedValue = edited.eGet(feature);
					Object modelValue = model.eGet(feature);
					
					String featureName = feature.getName();
					if (modelValue != null && !modelValue.equals(editedValue)) {
						System.out.println("Different "+featureName+" : "+editedValue+"/"+modelValue);
						model.eSet(feature, editedValue);
					}
				}
			}
		}
		
	}
}
