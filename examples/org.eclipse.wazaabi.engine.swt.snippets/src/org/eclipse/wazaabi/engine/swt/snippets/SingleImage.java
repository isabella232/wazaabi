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
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class SingleImage {
	public static void main(String[] args) {

		// create the display

		// setDisplay(new Display());
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		// create the shell which will receive the wazaabi components
		mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);
		// init SWT Engine in standalone mode
		SWTHelper.init(viewer);
		// init the 'urn:java' resolver
		URNJavaLocatorHelper.init(viewer);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();

		FillLayoutRule fillLayoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		fillLayoutRule.setPropertyName("layout"); //$NON-NLS-1$
		container.getStyleRules().add(fillLayoutRule);

		// create the label
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();

		// create the Image
		ImageRule imageRule = CoreStylesFactory.eINSTANCE.createImageRule();
		imageRule.setPropertyName("image"); //$NON-NLS-1$
		imageRule.setValue("urn:java:download.png");

		label.getStyleRules().add(imageRule);

		StringRule stringRule = CoreStylesFactory.eINSTANCE.createStringRule();
		stringRule.setPropertyName("text");
		stringRule.setValue("frefze");

		label.getStyleRules().add(stringRule);

		container.getChildren().add(label);

		// Set the content
		viewer.setContents(container);
		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();

		/*
		 * imageRule.setValue(URI2);
		 * 
		 * // get the current label image == second one secondSWTImage =
		 * swtLabel.getImage(); // the current label image is not null
		 * Assert.assertNotNull(secondSWTImage); // the current label image is
		 * not disposed Assert.assertFalse(secondSWTImage.isDisposed());
		 * 
		 * // the previous label image has been disposed
		 * Assert.assertTrue(firstSWTImage.isDisposed());
		 */
	}

}
