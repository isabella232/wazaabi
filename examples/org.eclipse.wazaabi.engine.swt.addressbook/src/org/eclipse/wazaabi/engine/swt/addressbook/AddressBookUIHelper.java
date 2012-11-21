package org.eclipse.wazaabi.engine.swt.addressbook;

import org.eclipse.wazaabi.mm.core.annotations.Annotation;
import org.eclipse.wazaabi.mm.core.annotations.AnnotationContent;
import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class AddressBookUIHelper {

	public static Container createMasterUI() {
		
		Container master = CoreWidgetsFactory.eINSTANCE.createContainer();
		master.setId("masterContainer");
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
		
		PathSelector pathSelector3 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector3.setPropertyName("content-provider");
		pathSelector3.setEClassifierName("Person");
		pathSelector3.getPaths().add("&children");
		
		collection.getStyleRules().add(pathSelector1);
		collection.getStyleRules().add(pathSelector2);
		collection.getStyleRules().add(pathSelector3);

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
		PropertyChangedEvent ev = EDPEventsFactory.eINSTANCE.createPropertyChangedEvent();
		ev.setPath("&selection");
		eventHandler.getEvents().add(ev);
		eventHandler.getExecutables().add(action);
		
		
		collection.getHandlers().add(eventHandler);
		
		master.getChildren().add(collection);
		
		return master;
	}

	public static Container createPersonDetailUI() {
		Container detail = CoreWidgetsFactory.eINSTANCE.createContainer();
		detail.setId("detailContainer");
		
		GridLayoutRule grid = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
		grid.setPropertyName("layout");
		grid.setNumColumns(2);
		grid.setHorizontalSpacing(0);
		grid.setVerticalSpacing(10);
		detail.getStyleRules().add(grid);
				

		
		Label label1 = CoreWidgetsFactory.eINSTANCE.createLabel();
		label1.setText("First name");
		
		GridDataRule gridDataL1 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gridDataL1.setPropertyName("layout-data");
		label1.getStyleRules().add(gridDataL1);
		detail.getChildren().add(label1);
		
		TextComponent text1 = CoreWidgetsFactory.eINSTANCE.createTextComponent();
		
		Binding bindingUi2m1 = EDPHandlersFactory.eINSTANCE.createBinding();
		Event eventUi2m1 = EDPEventsFactory.eINSTANCE.createEvent();
		eventUi2m1.setId("core:ui:text:modify");
		bindingUi2m1.getEvents().add(eventUi2m1);
		StringParameter sourceUi2m1 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		sourceUi2m1.setName("source");
		sourceUi2m1.setValue("@text");
		bindingUi2m1.getParameters().add(sourceUi2m1);
		StringParameter targetUi2m1 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		targetUi2m1.setName("target");
		targetUi2m1.setValue("$detailmodel/@firstName");
		bindingUi2m1.getParameters().add(targetUi2m1);
		text1.getHandlers().add(bindingUi2m1);
		
		Binding bindingM2ui1 = EDPHandlersFactory.eINSTANCE.createBinding();
		PropertyChangedEvent eventM2ui1 = EDPEventsFactory.eINSTANCE.createPropertyChangedEvent();
		eventM2ui1.setPath("$detailmodel/@firstName");
		bindingM2ui1.getEvents().add(eventM2ui1);
		Event eventM2uiRefresh1 = EDPEventsFactory.eINSTANCE.createEvent();
		eventM2uiRefresh1.setId("core:ui:refresh");
		bindingM2ui1.getEvents().add(eventM2uiRefresh1);
		StringParameter sourceM2ui1 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		sourceM2ui1.setName("source");
		sourceM2ui1.setValue("$detailmodel/@firstName");
		bindingM2ui1.getParameters().add(sourceM2ui1);
		StringParameter targetM2ui1 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		targetM2ui1.setName("target");
		targetM2ui1.setValue("@text");
		bindingM2ui1.getParameters().add(targetM2ui1);
		text1.getHandlers().add(bindingM2ui1);
		
		GridDataRule gridData1 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gridData1.setPropertyName("layout-data");
		gridData1.setGrabExcessHorizontalSpace(true);
		gridData1.setHorizontalAlignement(GridDataAlignment.FILL);
		gridData1.setHorizontalSpan(1);
		text1.getStyleRules().add(gridData1);
		
		BooleanRule border1 = CoreStylesFactory.eINSTANCE.createBooleanRule();
		border1.setPropertyName("border");
		border1.setValue(true);
		text1.getStyleRules().add(border1);
		detail.getChildren().add(text1);
		
		


		Label label2 = CoreWidgetsFactory.eINSTANCE.createLabel();
		label2.setText("Last name");
		
		GridDataRule gridDataL2 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gridDataL2.setPropertyName("layout-data");
		label2.getStyleRules().add(gridDataL2);
		detail.getChildren().add(label2);
		
		TextComponent text2 = CoreWidgetsFactory.eINSTANCE.createTextComponent();
		
		Binding bindingUi2m2 = EDPHandlersFactory.eINSTANCE.createBinding();
		Event eventUi2m2 = EDPEventsFactory.eINSTANCE.createEvent();
		eventUi2m2.setId("core:ui:text:modify");
		bindingUi2m2.getEvents().add(eventUi2m2);
		StringParameter sourceUi2m2 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		sourceUi2m2.setName("source");
		sourceUi2m2.setValue("@text");
		bindingUi2m2.getParameters().add(sourceUi2m2);
		StringParameter targetUi2m2 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		targetUi2m2.setName("target");
		targetUi2m2.setValue("$detailmodel/@lastName");
		bindingUi2m2.getParameters().add(targetUi2m2);
		text2.getHandlers().add(bindingUi2m2);
		
		Binding bindingM2ui2 = EDPHandlersFactory.eINSTANCE.createBinding();
		PropertyChangedEvent eventM2ui2 = EDPEventsFactory.eINSTANCE.createPropertyChangedEvent();
		eventM2ui2.setPath("$detailmodel/@lastName");
		bindingM2ui2.getEvents().add(eventM2ui2);
		Event eventM2uiRefresh2 = EDPEventsFactory.eINSTANCE.createEvent();
		eventM2uiRefresh2.setId("core:ui:refresh");
		bindingM2ui2.getEvents().add(eventM2uiRefresh2);
		StringParameter sourceM2ui2 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		sourceM2ui2.setName("source");
		sourceM2ui2.setValue("$detailmodel/@lastName");
		bindingM2ui2.getParameters().add(sourceM2ui2);
		StringParameter targetM2ui2 = EDPHandlersFactory.eINSTANCE.createStringParameter();
		targetM2ui2.setName("target");
		targetM2ui2.setValue("@text");
		bindingM2ui2.getParameters().add(targetM2ui2);
		text2.getHandlers().add(bindingM2ui2);
		
		GridDataRule gridData2 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gridData2.setPropertyName("layout-data");
		gridData2.setGrabExcessHorizontalSpace(true);
		gridData2.setHorizontalAlignement(GridDataAlignment.FILL);
		gridData2.setHorizontalSpan(1);
		text2.getStyleRules().add(gridData2);
		
		BooleanRule border2 = CoreStylesFactory.eINSTANCE.createBooleanRule();
		border2.setPropertyName("border");
		border2.setValue(true);
		text2.getStyleRules().add(border2);
		detail.getChildren().add(text2);
		
		return detail;
	}
	
	public static Container createEmptyDetailUI() {
		Container detail = CoreWidgetsFactory.eINSTANCE.createContainer();
		detail.setId("detailContainer");
		
		GridLayoutRule grid = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
		grid.setPropertyName("layout");
		grid.setNumColumns(2);
		detail.getStyleRules().add(grid);
		
		return detail;
	}

}
