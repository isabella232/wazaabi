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
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Slider;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class TwoSlidersInAContainerWithBinding {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();

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

		// create a first Slider
		Slider slider1 = CoreWidgetsFactory.eINSTANCE.createSlider();
		slider1.setValue(50);
		slider1.setOrientation(Orientation.VERTICAL);


		// create a second Slider
		final Slider slider2 = CoreWidgetsFactory.eINSTANCE.createSlider();

		// append the sliders to the container's children list.
		container.getChildren().add(slider1);
		container.getChildren().add(slider2);

		Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
		StringParameter source = EDPHandlersFactory.eINSTANCE.createStringParameter();
		StringParameter target = EDPHandlersFactory.eINSTANCE.createStringParameter();
		source.setName("source");
		source.setValue("@value");
		target.setName("target");
		target.setValue("../Slider[1]/@value");
		binding.getParameters().add(source);
		binding.getParameters().add(target);

		slider1.getHandlers().add(binding);
		PropertyChangedEvent event = EDPEventsFactory.eINSTANCE.createPropertyChangedEvent();
		binding.getEvents().add(event);
		event.setPath("@value");

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
