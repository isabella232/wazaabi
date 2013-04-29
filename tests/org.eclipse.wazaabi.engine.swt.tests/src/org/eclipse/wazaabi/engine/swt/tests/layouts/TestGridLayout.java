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

package org.eclipse.wazaabi.engine.swt.tests.layouts;

import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.junit.Assert;
import org.junit.Test;

public class TestGridLayout extends AbstractTestGridLayout {

	@Test
	public void addButtonAfterSetContents() {
		createWazaabiGridLayout(false, "");
		createSWTGridLayoutTwoButtons("");
		getViewer().getUpdateManager().performUpdate();

	
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();		
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

	@Test
	public void addButtonBeforeSetContents () {
		createWazaabiGridLayout(true, "");
		createSWTGridLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

	@Test
	public void addButtonAfterSetContentsWithRowData () {
		createWazaabiGridLayout(false, "GridData");
		createSWTGridLayoutTwoButtons("GridData");
		getViewer().getUpdateManager().performUpdate();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

	@Test
	public void addButtonBeforeSetContentsWithRowData () {
		createWazaabiGridLayout(true, "GridData");
		createSWTGridLayoutTwoButtons("GridData");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	
	@Test
	public void removeButtonAfterSetContents() {
		createWazaabiGridLayoutAndRemoveButtonTest(false, "");
		createSWTGridLayoutOneButton("");

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	@Test
	public void removeButtonBeforeSetContents() {
		createWazaabiGridLayoutAndRemoveButtonTest(true, "");
		createSWTGridLayoutOneButton("");

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	@Test
	public void removeButtonAfterSetContentsWithRowData() {
		createWazaabiGridLayoutAndRemoveButtonTest(false, "RowData");
		createSWTGridLayoutOneButton("RowData");

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	@Test
	public void removeButtonBeforeSetContentsWithRowData() {
		createWazaabiGridLayoutAndRemoveButtonTest(true, "RowData");
		createSWTGridLayoutOneButton("RowData");

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	
	
	
	@Test
	public void testModelSetMultipleGridLayoutBeforeSetContents () {
		testModelSetMultipleLayout(true);
		createSWTGridLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelSetMultipleGridLayoutAfterSetContents () {
		testModelSetMultipleLayout(false);
		createSWTGridLayoutTwoButtons("");
		getViewer().getUpdateManager().performUpdate();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelMoveGridLayoutBeforeSetContents () {
		testModelMoveLayout(true);
		createSWTGridLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelMoveGridLayoutAfterSetContents () {
		testModelMoveLayout(false);
		createSWTGridLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveGridLayoutByRemoveBeforeSetContents () {
		testModelRemoveLayoutByRemove(true);
		createSWTGridLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveGridLayoutByRemoveAfterSetContents () {
		testModelRemoveLayoutByRemove(false);
		createSWTGridLayoutTwoButtons("");
		getViewer().getUpdateManager().performUpdate();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveGridLayoutByRenameBeforeSetContents () {
		testModelRemoveLayoutByRename(true);
		createSWTGridLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveGridLayoutByRenameAfterSetContents () {
		testModelRemoveLayoutByRename(false);
		createSWTGridLayoutTwoButtons("");
		getViewer().getUpdateManager().performUpdate();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	
}
