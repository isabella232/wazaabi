package org.eclipse.wazaabi.engine.swt.addressbook;

import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class AddressBookUIHelper {

	public static Container createMasterUI() {
		
		Container master = CoreWidgetsFactory.eINSTANCE.createContainer();
		FillLayoutRule fill = SWTStylesFactory.eINSTANCE.createFillLayoutRule();
		fill.setPropertyName("layout");
		master.getStyleRules().add(fill);
		
		Collection collection = CoreWidgetsFactory.eINSTANCE.createCollection();
		
		Annotation inputAnnotation = CoreAnnotationsFactory.eINSTANCE.createAnnotation();
		inputAnnotation.setSource("http://www.wazaabi.org/set-feature");
		AnnotationContent cont1 = CoreAnnotationsFactory.eINSTANCE.createAnnotationContent();
		cont1.setKey("feature-name");
		cont1.setValue("input");
		AnnotationContent cont2 = CoreAnnotationsFactory.eINSTANCE.createAnnotationContent();
		cont2.setKey("type");
		cont2.setValue("locationpath");
		AnnotationContent cont3 = CoreAnnotationsFactory.eINSTANCE.createAnnotationContent();
		cont3.setKey("value");
		cont3.setValue("$datamodel");
		inputAnnotation.getContents().add(cont1);
		inputAnnotation.getContents().add(cont2);
		inputAnnotation.getContents().add(cont3);
		collection.getAnnotations().add(inputAnnotation);
		
		LookAndFeelRule lookAndFeelRule = CoreCollectionsStylesFactory.eINSTANCE
				.createLookAndFeelRule();
		lookAndFeelRule.setPropertyName("lookandfeel"); //$NON-NLS-1$
		lookAndFeelRule.setValue(LookAndFeel.TREE);
		collection.getStyleRules().add(lookAndFeelRule);
		
		PathSelector pathSelector1 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector1.setPropertyName("content-provider");
		pathSelector1.setEClassifierName("AddressBook");
		pathSelector1.getPaths().add("&persons");
		
		PathSelector pathSelector2 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector2.setPropertyName("label-renderer");
		pathSelector2.setEClassifierName("Person");
		pathSelector2.getPaths().add("@firstName");
		pathSelector2.getPaths().add("@lastName");
		
		collection.getStyleRules().add(pathSelector1);
		collection.getStyleRules().add(pathSelector2);

		WeightedColumnDescriptor columnDescriptor1 = CoreCollectionsStylesFactory.eINSTANCE
				.createWeightedColumnDescriptor();
		columnDescriptor1.setLabel("First Name");
		columnDescriptor1.setPropertyName("column-descriptor");
		columnDescriptor1.setWeight(100);
		
		WeightedColumnDescriptor columnDescriptor2 = CoreCollectionsStylesFactory.eINSTANCE
				.createWeightedColumnDescriptor();
		columnDescriptor2.setLabel("Last Name");
		columnDescriptor2.setPropertyName("column-descriptor");
		columnDescriptor2.setWeight(100);

		collection.getStyleRules().add(columnDescriptor1);
		collection.getStyleRules().add(columnDescriptor2);
		
		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri("platform:/plugin/org.eclipse.wazaabi.engine.swt.addressbook/org.eclipse.wazaabi.engine.swt.addressbook.handlers.MasterToDetailHandler");
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId("core:ui:selection");
		eventHandler.getExecutables().add(action);
		eventHandler.getEvents().add(event);
		
		collection.getHandlers().add(eventHandler);
		
		master.getChildren().add(collection);
		
		return master;
	}

	public static Container createDetailUI() {
		Container detail = CoreWidgetsFactory.eINSTANCE.createContainer();
		FillLayoutRule fill = SWTStylesFactory.eINSTANCE.createFillLayoutRule();
		fill.setPropertyName("layout");
		detail.getStyleRules().add(fill);
		
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText("blabla");
		detail.getChildren().add(label);
		
		PushButton push = CoreWidgetsFactory.eINSTANCE.createPushButton();
		push.setText("brol");
		EventHandler handler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri("platform:/plugin/org.eclipse.wazaabi.engine.swt.addressbook/org.eclipse.wazaabi.engine.swt.addressbook.handlers.MasterToDetailHandler");
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId("core:ui:selection");
		handler.getExecutables().add(action);
		handler.getEvents().add(event);
		push.getHandlers().add(handler);
		
		detail.getChildren().add(push);
		
		return detail;
	}

}
