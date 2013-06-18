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
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;

public class PushInAMenu {

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
		
		
		MenuComponent topmenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		topmenu.setText("blabla");
		StringRule toptype = CoreStylesFactory.eINSTANCE.createStringRule();
		toptype.setPropertyName("type");
		toptype.setValue("top-bar");
		topmenu.getStyleRules().add(toptype);
		
		//after
		//viewer.setContents(topmenu);

		
		MenuComponent submenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		submenu.setText("Filee");
		StringRule subtype = CoreStylesFactory.eINSTANCE.createStringRule();
		subtype.setPropertyName("type");
		subtype.setValue("submenu");
		submenu.getStyleRules().add(subtype);
		
		topmenu.getChildren().add(submenu);
				
		MenuComponent pushmenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		pushmenu.setText("Bigbutton");
		StringRule pushtype = CoreStylesFactory.eINSTANCE.createStringRule();
		pushtype.setPropertyName("type");
		pushtype.setValue("push");
		pushmenu.getStyleRules().add(pushtype);
		
		submenu.getChildren().add(pushmenu);
		// inject the container into the viewer
		
		//before
		viewer.setContents(topmenu);

		mainShell.open();
		mainShell.getMenuBar();
		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
