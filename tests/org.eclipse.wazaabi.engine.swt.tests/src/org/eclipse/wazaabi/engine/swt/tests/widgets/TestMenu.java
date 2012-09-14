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

import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.junit.Assert;
import org.junit.Test;

public class TestMenu extends AbstractTestMenu {

//	@Test
//	public void showSWTWidget() {
//		createSWTMenu();
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();
//	}
//	
//
//	@Test
//	public void showWazaabiWidget() {
//		createWazaabiMenu(true);
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();
//	}

	@Test
	public void testModelMenuTextSetBeforeViewerSetContentsEqualsText() {

		createWazaabiMenu(true);

		org.eclipse.swt.widgets.MenuItem swtItem = (org.eclipse.swt.widgets.MenuItem) SWTUtils.getWidget(viewer, subMenu);
		Assert.assertEquals(SUB_TEXT, swtItem.getText());
	}
	
	@Test
	public void testModelMenuTextSetAfterViewerSetContentsEqualsText() {

		createWazaabiMenu(false);

		org.eclipse.swt.widgets.MenuItem swtItem = (org.eclipse.swt.widgets.MenuItem) SWTUtils.getWidget(viewer, subMenu);
		Assert.assertEquals(SUB_TEXT, swtItem.getText());
	}
	
	@Test
	public void testModelMenuTextSetBeforeViewerSetContentsEqualsSubMenuStructure() {

		createWazaabiMenu(true);
		
		org.eclipse.swt.widgets.Menu swtMenu2 = (org.eclipse.swt.widgets.Menu) SWTUtils.getWidget(viewer, menuBar);
		Assert.assertEquals(swtMenu2.getItemCount(), menuBar.getChildren().size());
	}
	
	@Test
	public void testModelMenuTextSetAfterViewerSetContentsEqualsSubMenuStructure() {

		createWazaabiMenu(false);

		org.eclipse.swt.widgets.Menu swtMenu2 = (org.eclipse.swt.widgets.Menu) SWTUtils.getWidget(viewer, menuBar);
		Assert.assertEquals(swtMenu2.getItemCount(), menuBar.getChildren().size());
	}

	@Test
	public void testModelMenuTextSetBeforeViewerSetContentsEqualsMenuStructure() {

		createWazaabiMenu(true);
		
		org.eclipse.swt.widgets.Menu swtMenu = (org.eclipse.swt.widgets.Menu) SWTUtils.getWidget(viewer, menuComponent);
		Assert.assertEquals(swtMenu.getItemCount(), menuComponent.getChildren().size());
	
	}
	
	@Test
	public void testModelMenuTextSetAfterViewerSetContentsEqualsMenuStructure() {

		createWazaabiMenu(false);
		
		org.eclipse.swt.widgets.Menu swtMenu = (org.eclipse.swt.widgets.Menu) SWTUtils.getWidget(viewer, menuComponent);
		Assert.assertEquals(swtMenu.getItemCount(), menuComponent.getChildren().size());
	}

}
