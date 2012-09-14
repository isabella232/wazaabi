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
import org.eclipse.wazaabi.coderesolution.groovy.embedded.script.codelocators.nonosgi.GroovyEmbeddedScriptHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class PushButtonWithHandlerInGroovyScript {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();

		// init the 'urn:java' resolver
		GroovyEmbeddedScriptHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// create a PushButton
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.setText("Hello World"); //$NON-NLS-1$

		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri("urn:groovy:embedded:script:System.out.println (\"event thrown: \" + event.getId())");
		pushButton.getHandlers().add(eventHandler);

		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		eventHandler.getEvents().add(event);
		event.setId("core:ui:selection");

		// inject the button into the viewer
		viewer.setContents(pushButton);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
