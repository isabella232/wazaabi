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

import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.SashRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;

public class PushButtonInASashForm {

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

		viewer.setContents(container);

		SashFormLayoutRule sashLayout = CoreStylesFactory.eINSTANCE
				.createSashFormLayoutRule();
		sashLayout.setPropertyName("layout");
		container.getStyleRules().add(sashLayout);
		sashLayout.setOrientation(Orientation.VERTICAL);

		SashRule sashRule1 = CoreStylesFactory.eINSTANCE.createSashRule();
		sashRule1.setPropertyName("layout-data");
		sashRule1.setWeight(10);

		// create a pushButton
		PushButton pushButton1 = CoreWidgetsFactory.eINSTANCE
				.createPushButton();
		pushButton1.setText("Hello World"); //$NON-NLS-1$
		// pushButton1.getStyleRules().add(sashRule1);

		Label label1 = CoreWidgetsFactory.eINSTANCE.createLabel();
		label1.setText("blublu");
		label1.getStyleRules().add(sashRule1);

		SashRule sashRule2 = CoreStylesFactory.eINSTANCE.createSashRule();
		sashRule2.setPropertyName("layout-data");
		sashRule2.setWeight(4);

		PushButton pushButton2 = CoreWidgetsFactory.eINSTANCE
				.createPushButton();
		pushButton2.setText("Ukulele"); //$NON-NLS-1$
		pushButton2.getStyleRules().add(sashRule2);

		SashRule sashRule3 = CoreStylesFactory.eINSTANCE.createSashRule();
		sashRule3.setPropertyName("layout-data");
		sashRule3.setWeight(2);

		Label label2 = CoreWidgetsFactory.eINSTANCE.createLabel();
		label2.setText("This is label test");
		label2.getStyleRules().add(sashRule3);

		// append the button to the container's children list.
		container.getChildren().add(pushButton1);
		container.getChildren().add(label1);
		container.getChildren().add(pushButton2);
		container.getChildren().add(label2);

		// inject the container into the viewer

		Resource r = new XMIResourceImpl();
		r.getContents().add(container);
		try {
			r.save(System.out, null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
