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
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.DirectionRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;

public class PushInAContextMenu {

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
		
		MenuComponent topmenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		StringRule toptype = CoreStylesFactory.eINSTANCE.createStringRule();
		toptype.setPropertyName("type");
		toptype.setValue("top-context");
		DirectionRule dir = CoreStylesFactory.eINSTANCE.createDirectionRule();
		dir.setPropertyName("direction");
		dir.setValue(Direction.RIGHT_TO_LEFT);
		topmenu.getStyleRules().add(dir);
		topmenu.getStyleRules().add(toptype);
				
		viewer.setContents(topmenu);
		
		MenuComponent pushmenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		pushmenu.setText("Bigbutton");
		StringRule pushtype = CoreStylesFactory.eINSTANCE.createStringRule();
		pushtype.setPropertyName("type");
		pushtype.setValue("push");
		pushmenu.getStyleRules().add(pushtype);
		
		topmenu.getChildren().add(pushmenu);
		
		MenuComponent submenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		submenu.setText("Submenu");
		StringRule subtype = CoreStylesFactory.eINSTANCE.createStringRule();
		subtype.setPropertyName("type");
		subtype.setValue("submenu");
		submenu.getStyleRules().add(subtype);
		
		topmenu.getChildren().add(submenu);
		
		MenuComponent checkmenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		checkmenu.setText("checkme");
		StringRule checktype = CoreStylesFactory.eINSTANCE.createStringRule();
		checktype.setPropertyName("type");
		checktype.setValue("check");
		checkmenu.getStyleRules().add(checktype);
		
		submenu.getChildren().add(checkmenu);
		
		MenuComponent subsubmenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		subsubmenu.setText("Submenu2");
		StringRule subsubtype = CoreStylesFactory.eINSTANCE.createStringRule();
		subsubtype.setPropertyName("type");
		subsubtype.setValue("submenu");
		subsubmenu.getStyleRules().add(subsubtype);
		
		
		
		MenuComponent checksubmenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		checksubmenu.setText("checkmetoo");
		StringRule subchecktype = CoreStylesFactory.eINSTANCE.createStringRule();
		subchecktype.setPropertyName("type");
		subchecktype.setValue("check");
		checksubmenu.getStyleRules().add(subchecktype);
		
		subsubmenu.getChildren().add(checksubmenu);
		submenu.getChildren().add(subsubmenu);
		
		
		// inject the container into the viewer
		
		

		mainShell.open();
		mainShell.getMenuBar();
		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
