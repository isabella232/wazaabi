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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class ViewerWith2SetContents {

	public static void main(String[] args) {

		// init SWT Engine in standalone mode
		SWTHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		final SWTControlViewer viewer = new SWTControlViewer(mainShell);

		mainShell.open();

		// inject the container into the viewer
//		viewer.setContents(getUi1());

		Display.getCurrent().timerExec(2000, new Runnable() {

			public void run() {
				viewer.setContents(getUi2());
			}
		});

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	public static EObject getUi1() {
		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();

		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create a pushButton
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.setText("B1"); //$NON-NLS-1$

		// append the button to the container's children list.
		container.getChildren().add(pushButton);
		return container;
	}

	public static EObject getUi2() {
		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();

		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create a pushButton
		for (int i = 0; i < 10; i++) {
			PushButton pushButton = CoreWidgetsFactory.eINSTANCE
					.createPushButton();
			pushButton.setText("B2-" + i); //$NON-NLS-1$

			// append the button to the container's children list.
			container.getChildren().add(pushButton);
		}
		return container;
	}
}
