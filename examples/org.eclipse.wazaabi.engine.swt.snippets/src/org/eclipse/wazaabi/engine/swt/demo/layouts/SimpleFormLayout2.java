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

package org.eclipse.wazaabi.engine.swt.demo.layouts;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.commons.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer;
import org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling;
import org.eclipse.wazaabi.mm.swt.styles.FormDataRule;
import org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class SimpleFormLayout2 {

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

		// create a composite and set its layout
		Container composite = CoreWidgetsFactory.eINSTANCE.createContainer();
		FormLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createFormLayoutRule();
		layoutRule.setPropertyName("layout");
		composite.getStyleRules().add(layoutRule);
		// layoutRule.setMarginHeight(30);

		PushButton button1 = CoreWidgetsFactory.eINSTANCE
				.createPushButton();
		button1.setText("B1");
		button1.setId("button1");
		FormDataRule formDataRule1 = SWTStylesFactory.eINSTANCE
				.createFormDataRule();
		formDataRule1.setPropertyName("layout-data");
		AttachmentToContainer attachment1 = SWTStylesFactory.eINSTANCE
				.createAttachmentToContainer();
		attachment1.setNumerator(20);
		formDataRule1.setTop(attachment1);

		button1.getStyleRules().add(formDataRule1);


		PushButton button2 = CoreWidgetsFactory.eINSTANCE
				.createPushButton();
		button2.setText("Wide Button 2");
		FormDataRule formDataRule2 = SWTStylesFactory.eINSTANCE
				.createFormDataRule();
		formDataRule2.setPropertyName("layout-data");
		AttachmentToSibling attachment2 = SWTStylesFactory.eINSTANCE
				.createAttachmentToSibling();
		attachment2.setSiblingId(button1.getId());
		formDataRule2.setTop(attachment2);

		button2.getStyleRules().add(formDataRule2);

		composite.getChildren().add(button1);
		composite.getChildren().add(button2);

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
