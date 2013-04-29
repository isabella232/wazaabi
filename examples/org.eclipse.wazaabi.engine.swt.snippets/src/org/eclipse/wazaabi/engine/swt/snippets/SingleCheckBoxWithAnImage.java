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
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.CheckBox;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class SingleCheckBoxWithAnImage {
	public static void main(String[] arsg) {

		final String URI1 = "Idea.jpg"; //$NON-NLS-1$

		// init SWT Engine in standalone mode
		SWTHelper.init();
		// init the 'urn:java' resolver
		URNJavaLocatorHelper.init();

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);
		viewer.setCodeLocatorBaseUri("urn:java:");

		// create a checkBox
		// checkBox.setText("checked");

		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();

		FillLayoutRule fillLayoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		fillLayoutRule.setPropertyName("layout"); //$NON-NLS-1$
		container.getStyleRules().add(fillLayoutRule);

		CheckBox checkbox = CoreWidgetsFactory.eINSTANCE.createCheckBox();
		checkbox.setImage(URI1);

		container.getChildren().add(checkbox);

		// inject the button into the viewer
		viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();

	}

}
