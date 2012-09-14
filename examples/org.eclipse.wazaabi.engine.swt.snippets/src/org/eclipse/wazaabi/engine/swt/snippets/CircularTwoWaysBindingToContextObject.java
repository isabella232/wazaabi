/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.snippets;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.coderesolution.reflection.java.codelocators.nonosgi.ReflectionJavaHelper;
import org.eclipse.wazaabi.engine.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class CircularTwoWaysBindingToContextObject {

	public static void main(String[] args) {

		// initialize SWT Engine in standalone mode
		SWTHelper.init();

		// initialize the 'urn:java' resolver
		ReflectionJavaHelper.init();

		// initialize the locationPaths processor
		LocationPathsHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create two TextComponents
		TextComponent text1 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		text1.setText("value ....");
		TextComponent text2 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		text2.setText("....");

		PushButton button = CoreWidgetsFactory.eINSTANCE.createPushButton();
		StringRule textRule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
				.createStringRule();
		textRule.setPropertyName("text");//$NON-NLS-1$
		textRule.setValue("Change context...");//$NON-NLS-1$
		button.getStyleRules().add(textRule);
		
		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		button.getHandlers().add(eventHandler);

		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri("urn:java:org.eclipse.wazaabi.engine.swt.snippets.handlers.TwoWaysBindingToContextObjectAction");
		eventHandler.getExecutables().add(action);

		Event selectionEvent = EDPEventsFactory.eINSTANCE.createEvent();
		eventHandler.getEvents().add(selectionEvent);
		selectionEvent.setId("core:ui:selection");

		final EAttribute attr = EcoreFactory.eINSTANCE.createEAttribute();
		attr.setName("hello");
		container.set("contextObject", attr);
		attr.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				if (msg.getFeature() instanceof EAttribute
						&& "name".equals(((EAttribute) msg.getFeature())
								.getName()))
					System.out.println("name changed " + attr.getName());
			}

		});

		// append textComponents to container's children list.
		container.getChildren().add(text1);
		container.getChildren().add(text2);
		container.getChildren().add(button);

		Binding uiToDomainBinding = EDPHandlersFactory.eINSTANCE
				.createBinding();
		StringParameter source = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		StringParameter target = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		source.setName("source");
		source.setValue("@text");
		target.setName("target");
		target.setValue("$contextObject/@name");
		uiToDomainBinding.getParameters().add(source);
		uiToDomainBinding.getParameters().add(target);

		PropertyChangedEvent propertyChangeEvent1 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		propertyChangeEvent1.setPath("@text");
		uiToDomainBinding.getEvents().add(propertyChangeEvent1);

		text1.getHandlers().add(uiToDomainBinding);

		Binding domainToUiBinding = EDPHandlersFactory.eINSTANCE
				.createBinding();
		source = EDPHandlersFactory.eINSTANCE.createStringParameter();
		target = EDPHandlersFactory.eINSTANCE.createStringParameter();
		source.setName("source");
		source.setValue("$contextObject/@name");
		target.setName("target");
		target.setValue("@text");
		domainToUiBinding.getParameters().add(source);
		domainToUiBinding.getParameters().add(target);

		PropertyChangedEvent propertyChangeEvent2 = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		propertyChangeEvent2.setPath("$contextObject/@name");
		domainToUiBinding.getEvents().add(propertyChangeEvent2);

		text1.getHandlers().add(domainToUiBinding);

		// Set the content
		viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
