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

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class OneWayBinding {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();

		// initialize the locationPaths processor
		LocationPathsHelper.init();

		// // init the 'urn:java' resolver
		// ReflectionJavaHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// create a composite and set its layout
		Container composite = CoreWidgetsFactory.eINSTANCE.createContainer();
		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();
		layoutRule.setPropertyName("layout");
		composite.getStyleRules().add(layoutRule);

		// create two TextComponents
		TextComponent text1 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		text1.setText("text 1");
		TextComponent text2 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		text2.setText("text 2");

		// append textComponents to container's children list.
		composite.getChildren().add(text1);
		composite.getChildren().add(text2);

		Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
		StringParameter source = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		StringParameter target = EDPHandlersFactory.eINSTANCE
				.createStringParameter();
		source.setName("source");
		source.setValue("@text");
		target.setName("target");
		target.setValue("../TextComponent[1]/@text");
		binding.getParameters().add(source);
		binding.getParameters().add(target);

		text1.getHandlers().add(binding);
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		binding.getEvents().add(event);
		event.setId("core:ui:focus:out");

		// Set the content
		viewer.setContents(composite);

		
//		Resource r = new XMIResource () ;
//		r.getContents().add(composite);
//		r.save(System.out, null);
//		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
