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
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.Direction;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class LabelInAContainer {

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

		// create a Label
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText("Hello World"); //$NON-NLS-1$

		
		label.setToolTipText("this is my super tool tip text");
		label.setErrorText("This is my error text");
		
		label.setDirection(Direction.RIGHT_TO_LEFT);
		
		
		
		// append the button to the container's children list.
		container.getChildren().add(label);

		// inject the container into the viewer
		viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
		
//		Display display2 = new Display();
//		// create the shell which will receive the pure SWT components
//		Shell swtShell = new Shell(display2, SWT.SHELL_TRIM);
//		swtShell.setText("SWT");
//		swtShell.setLayout(new FillLayout());
//		swtShell.setSize(300, 300);
//
//		// create the content
//		Composite swtComposite = new Composite(swtShell, SWT.NONE);
//		swtComposite.setLayout(new FillLayout());
//		
//		Button swtButton1 = new Button(swtComposite, SWT.PUSH);
//		swtButton1.setText("hello");
//		
//		org.eclipse.swt.widgets.Label label2 = new org.eclipse.swt.widgets.Label(swtComposite, SWT.RIGHT_TO_LEFT);
//		label2.setText("my label");
//		
//		swtShell.open();
//		
//		while (!swtShell.isDisposed()) {
//			if (!display2.readAndDispatch())
//				display2.sleep();
//		}
//		display2.dispose();
	}
}
