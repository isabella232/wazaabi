/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
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
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class TextComponentWithKeyEvent {

	public static void main(String[] args) {


		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// init SWT Engine in standalone mode
		SWTHelper.init(viewer);
		URNJavaLocatorHelper.init(viewer);
		
		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		TextComponent text = CoreWidgetsFactory.eINSTANCE.createTextComponent();

		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE
				.createEventHandler();

		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri("urn:java:org.eclipse.wazaabi.engine.swt.snippets.handlers.VerySimpleKeyEvent");
		text.getHandlers().add(eventHandler);

		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		eventHandler.getEvents().add(event);
		eventHandler.getExecutables().add(action);
		event.setId("core:ui:key:down");

		text.getHandlers().add(eventHandler);

		// append the button to the container's children list.
		container.getChildren().add(text);

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
