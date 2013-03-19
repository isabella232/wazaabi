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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.coderesolution.reflection.java.codelocators.nonosgi.ReflectionJavaHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.snippets.handlers.ChangeStackLayoutTopValue;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class StackLayout {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();
		// init the 'urn:java' resolver
		ReflectionJavaHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		StackLayoutRule layoutRule = CoreStylesFactory.eINSTANCE
				.createStackLayoutRule();
		layoutRule.setPropertyName("layout");

		// set the button1 as top component
		layoutRule.setTop(0);

		container.getStyleRules().add(layoutRule);

		// create a first pushButton
		PushButton pushButton1 = CoreWidgetsFactory.eINSTANCE
				.createPushButton();
		pushButton1.setText("Button1"); //$NON-NLS-1$
		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE
				.createEventHandler();
		eventHandler.setUri(ChangeStackLayoutTopValue.class.getName());

		pushButton1.getHandlers().add(eventHandler);
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		eventHandler.getEvents().add(event);
		event.setId("core:ui:selection");

		viewer.setCodeLocatorBaseUri("urn:java:");
		// append the button to the container's children list.
		container.getChildren().add(pushButton1);

		// create a first pushButton
		PushButton pushButton2 = CoreWidgetsFactory.eINSTANCE
				.createPushButton();
		pushButton2.setText("Button2"); //$NON-NLS-1$

		// append the button to the container's children list.
		container.getChildren().add(pushButton2);

		// inject the container into the viewer
		viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
