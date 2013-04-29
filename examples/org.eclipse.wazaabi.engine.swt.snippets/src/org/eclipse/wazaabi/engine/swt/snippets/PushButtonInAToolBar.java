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
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BarLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;

public class PushButtonInAToolBar {

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

		// create a pushButton
		PushButton pushButton1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton1.setText("Hello World"); //$NON-NLS-1$
		PushButton pushButton2 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton2.setText("Second button"); //$NON-NLS-1$
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText("Wazaabi");

		// append the button to the container's children list.
		container.getChildren().add(pushButton1);
		container.getChildren().add(pushButton2);
		container.getChildren().add(label);
		
		FontRule font = CoreStylesFactory.eINSTANCE.createFontRule();
		font.setBold(true);
		font.setItalic(true);
		font.setPropertyName("font");
		container.getStyleRules().add(font);


		
		BarLayoutRule layoutRule = CoreStylesFactory.eINSTANCE.createBarLayoutRule();
		layoutRule.setPropertyName("layout");
		layoutRule.setDraggable(false);
		container.getStyleRules().add(layoutRule);
		
		OrientationRule orientation = CoreStylesFactory.eINSTANCE.createOrientationRule();
		orientation.setPropertyName("orientation");
		orientation.setValue(Orientation.VERTICAL);
		container.getStyleRules().add(orientation);
		
		


		// inject the container into the viewer
		//viewer.setContents(container);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
