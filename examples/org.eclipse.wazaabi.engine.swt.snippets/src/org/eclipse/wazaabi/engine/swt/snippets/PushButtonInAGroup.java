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
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class PushButtonInAGroup {

	public static void main(String[] args) {

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);

		// init SWT Engine in standalone mode
		SWTHelper.init(viewer);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();

		// inject the container into the viewer
		viewer.setContents(container);

		RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createRowLayoutRule();
		ColorRule color = CoreStylesFactory.eINSTANCE.createColorRule();
		color.setBlue(50);
		layoutRule.setPropertyName("layout");
		StringRule group = CoreStylesFactory.eINSTANCE.createStringRule();
		group.setPropertyName("title-value");
		group.setValue("my group");
		BooleanRule border = CoreStylesFactory.eINSTANCE.createBooleanRule();
		border.setPropertyName("title-border");
		border.setValue(true);
		container.getStyleRules().add(border);
		container.getStyleRules().add(group);
		container.getStyleRules().add(layoutRule);

		// create a label
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText("my label");

		// create a pushButton
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.setText("Hello World"); //$NON-NLS-1$
		pushButton.getStyleRules().add(color);

		// append the button to the container's children list.
		container.getChildren().add(pushButton);
		container.getChildren().add(label);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
