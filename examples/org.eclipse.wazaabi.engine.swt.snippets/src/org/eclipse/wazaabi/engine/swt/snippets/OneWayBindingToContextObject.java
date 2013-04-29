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
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class OneWayBindingToContextObject {

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
		text1.setText("value ....");
		RowDataRule rowData1 = SWTStylesFactory.eINSTANCE.createRowDataRule();
		rowData1.setPropertyName("layout-data");
		rowData1.setWidth(80);
		text1.getStyleRules().add(rowData1);

		TextComponent text2 = CoreWidgetsFactory.eINSTANCE
				.createTextComponent();
		text2.setText("....");
		RowDataRule rowData2 = SWTStylesFactory.eINSTANCE.createRowDataRule();
		rowData2.setPropertyName("layout-data");
		rowData2.setWidth(80);
		text2.getStyleRules().add(rowData2);

		final EAttribute attr = EcoreFactory.eINSTANCE.createEAttribute();
		text1.set("contextObject", attr);
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
		target.setValue("$contextObject/@name");
		binding.getParameters().add(source);
		binding.getParameters().add(target);

		text1.getHandlers().add(binding);
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		binding.getEvents().add(event);
		event.setId("core:ui:focus:out");

		// Set the content
		viewer.setContents(composite);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
