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
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Spinner;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class SpinnerInAContainer {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();

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
		final Spinner spinner1 = CoreWidgetsFactory.eINSTANCE.createSpinner();
		//spinner1.setValue(0);
		spinner1.setMaximum(-100);
		spinner1.setMinimum(-150);
		//spinner1.setDigits(2);
		spinner1.setTextLimit(5);
		
		BooleanRule border = CoreStylesFactory.eINSTANCE.createBooleanRule();
		border.setPropertyName("border");
		border.setValue(true);
		spinner1.getStyleRules().add(border);
		
		FontRule font = CoreStylesFactory.eINSTANCE.createFontRule();
		font.setPropertyName("font");
		font.setHeight(90);
		font.setName("Comic sans MS");
		font.setItalic(true);
		font.setBold(true);
		spinner1.getStyleRules().add(font);
		
		
		spinner1.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				switch (msg.getFeatureID(Spinner.class)) {
				case CoreWidgetsPackage.SPINNER__VALUE:
					int min = spinner1.getMinimum();
					int max = spinner1.getMaximum();
					System.out.println("---> " + msg.getNewIntValue() + "[" + min + ";" + max + "]");
					break;
				}
			}

		});
		
		
		
		container.getChildren().add(spinner1);

		// create a second Slider
		
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
