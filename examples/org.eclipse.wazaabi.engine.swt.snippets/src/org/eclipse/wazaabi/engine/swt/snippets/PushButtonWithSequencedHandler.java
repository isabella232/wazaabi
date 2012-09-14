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
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Sequence;

public class PushButtonWithSequencedHandler {
	
	public static final String SIMPLE = "urn:java:org.eclipse.wazaabi.engine.swt.snippets.handlers.VerySimpleAction";
	public static final String SIMPLE2 = "urn:java:org.eclipse.wazaabi.engine.swt.snippets.handlers.SecondVerySimpleAction";
	
	public static final String JAVA_URN_TEST_URI1 = "urn:java:org.eclipse.wazaabi.test.testbundle.handlers.FirstTestHandler"; //$NON-NLS-1$ 
	public static final String JAVA_URN_TEST_URI2 = "urn:java:org.eclipse.wazaabi.test.testbundle.handlers.SecondTestHandler"; //$NON-NLS-1$ 

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

		// create a PushButton
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.setText("Hello World"); //$NON-NLS-1$

		Action action1 = EDPHandlersFactory.eINSTANCE.createAction();
		action1.setUri(SIMPLE);

		Action action2 = EDPHandlersFactory.eINSTANCE.createAction();
		action2.setUri(SIMPLE2);
		viewer.setContents(pushButton);
		
		Sequence seq = EDPHandlersFactory.eINSTANCE.createSequence();
		seq.getExecutables().add(action2);
				
		EventHandler eh = EDPHandlersFactory.eINSTANCE.createEventHandler();
		
		
		pushButton.getHandlers().add(eh);
		
		eh.getExecutables().add(action1);
		eh.getExecutables().add(seq);

		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		eh.getEvents().add(event);
		event.setId("core:ui:selection");

		// inject the button into the viewer
		

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
