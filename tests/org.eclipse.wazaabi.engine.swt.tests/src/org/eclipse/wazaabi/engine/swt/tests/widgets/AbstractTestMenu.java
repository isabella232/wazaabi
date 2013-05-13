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

package org.eclipse.wazaabi.engine.swt.tests.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.tests.AbstractCommandTest;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;

public abstract class AbstractTestMenu extends AbstractCommandTest {
	
	protected MenuComponent menuComponent;
	protected MenuComponent menuBar;
	protected MenuComponent subMenu;
	protected Menu swtMenuBar;
	protected Menu swtSubMenu;
	
	final protected String BAR_TEXT = "&File";
	final protected String BAR_TEXT2 = "&Wazaabi";
	final protected String SUB_TEXT = "Don't worry";
	final protected String SUB_TEXT2 = "Be happy";
	
	@Override
	public void before() {
		super.before();
		// create the PushButton
		
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	public void createWazaabiMenu (boolean before) {
		
		menuComponent = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		StringRule toptype = CoreStylesFactory.eINSTANCE.createStringRule();
		toptype.setPropertyName("type");
		toptype.setValue("top-bar");
		menuComponent.getStyleRules().add(toptype);
		
		if (!before)
			viewer.setContents(menuComponent);
		
		menuBar = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		menuBar.setText(BAR_TEXT);
		StringRule subMenuType = CoreStylesFactory.eINSTANCE.createStringRule();
		subMenuType.setPropertyName("type");
		subMenuType.setValue("submenu");
		menuBar.getStyleRules().add(subMenuType);
		
		MenuComponent fileItem2 = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		fileItem2.setText(BAR_TEXT2);
		StringRule itemType = CoreStylesFactory.eINSTANCE.createStringRule();
		itemType.setPropertyName("type");
		itemType.setValue("push");
		fileItem2.getStyleRules().add(itemType);
		
		menuComponent.getChildren().add(menuBar);
		menuComponent.getChildren().add(fileItem2);
		
		subMenu = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		subMenu.setText(SUB_TEXT);
		StringRule pushtype = CoreStylesFactory.eINSTANCE.createStringRule();
		pushtype.setPropertyName("type");
		pushtype.setValue("push");
		subMenu.getStyleRules().add(pushtype);
		
		MenuComponent item2 = CoreWidgetsFactory.eINSTANCE.createMenuComponent();
		item2.setText(SUB_TEXT2);
		StringRule item2type = CoreStylesFactory.eINSTANCE.createStringRule();
		item2type.setPropertyName("type");
		item2type.setValue("push");
		item2.getStyleRules().add(item2type);
		
		menuBar.getChildren().add(subMenu);
		menuBar.getChildren().add(item2);
		
		if (before)
			viewer.setContents(menuComponent);
		
		mainShell.open();
	}
	
	public void createSWTMenu () {
		mainShell = new Shell (display);
		mainShell.setSize (INITIAL_WIDTH, INITIAL_HEIGHT);
		
		swtMenuBar = new Menu (mainShell, SWT.BAR);
		mainShell.setMenuBar (swtMenuBar);
		
		MenuItem fileItem = new MenuItem (swtMenuBar, SWT.CASCADE);
		fileItem.setText (BAR_TEXT);
		
		MenuItem fileItem2 = new MenuItem (swtMenuBar, SWT.PUSH);
		fileItem2.setText (BAR_TEXT2);
		
		swtSubMenu = new Menu (mainShell, SWT.DROP_DOWN);
		fileItem.setMenu (swtSubMenu);
		
		MenuItem item = new MenuItem (swtSubMenu, SWT.PUSH);
		item.setText (SUB_TEXT);
		
		MenuItem item2 = new MenuItem (swtSubMenu, SWT.PUSH);
		item2.setText (SUB_TEXT2);
		
		mainShell.open ();
	}

}
