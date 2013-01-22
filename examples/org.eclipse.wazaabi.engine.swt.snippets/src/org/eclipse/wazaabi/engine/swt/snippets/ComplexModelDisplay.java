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
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.IntRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

/**
 * This example show how to display a single PushButton into a SWTViewer in a
 * standalone java application. The PushButton is created dynamically and
 * injected to the viewer before the display of the shell.
 * 
 * @author Olivier
 * 
 */
public class ComplexModelDisplay {

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

		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		// RowLayoutRule layoutRule =
		// org.eclipse.wazaabi.mm.swt.styles.StylesFactory.eINSTANCE
		// .createRowLayoutRule();
		// GridLayoutRule layoutRule =
		// org.eclipse.wazaabi.mm.swt.styles.StylesFactory.eINSTANCE
		// .createGridLayoutRule();
		FillLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);
		// layoutRule.setNumColumns(3);
		layoutRule.setMarginHeight(30);
		createPushButtons(5, container, 0);
		// createContainers(3, container, 1);

		viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	protected static int createPushButtons(int count, Container parent,
			int level) {
		for (int i = 0; i < count; i++) {
			PushButton pushButton = CoreWidgetsFactory.eINSTANCE
					.createPushButton();
			StringRule textRule = CoreStylesFactory.eINSTANCE
					.createStringRule();
			textRule.setPropertyName("text");//$NON-NLS-1$
			textRule.setValue("Hello " + level);//$NON-NLS-1$
			pushButton.getStyleRules().add(textRule);

			IntRule tabIndexRule = CoreStylesFactory.eINSTANCE.createIntRule();
			tabIndexRule.setPropertyName("tab-index");
			tabIndexRule.setValue(count - i - 1);
			pushButton.getStyleRules().add(tabIndexRule);

			parent.getChildren().add(pushButton);
		}
		return level++;
	}

	protected static void createContainers(int count, Container parent,
			int level) {
		for (int i = 0; i < count; i++) {
			Container container = CoreWidgetsFactory.eINSTANCE
					.createContainer();
			RowLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
					.createRowLayoutRule();
			layoutRule.setPropertyName("layout");
			container.getStyleRules().add(layoutRule);
			parent.getChildren().add(container);
			createPushButtons(count, container, level);
		}
	}
}
