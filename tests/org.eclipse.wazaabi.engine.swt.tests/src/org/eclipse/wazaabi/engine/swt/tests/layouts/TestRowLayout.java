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

public class TestRowLayout extends AbstractTestRowLayout {
	
	
	@Test
	public void addButtonAfterSetContents() {
		createWazaabiRowLayout(false, "");
		createSWTRowLayoutTwoButtons("");
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
		createWazaabiRowLayout(true, "");
		createSWTRowLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

	@Test
	public void addButtonAfterSetContentsWithRowData () {
		createWazaabiRowLayout(false, "RowData");
		createSWTRowLayoutTwoButtons("RowData");
		getViewer().getUpdateManager().performUpdate();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

	@Test
	public void addButtonBeforeSetContentsWithRowData () {
		createWazaabiRowLayout(true, "RowData");
		createSWTRowLayoutTwoButtons("RowData");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void removeButtonAfterSetContents() {
		createWazaabiRowLayoutAndRemoveButtonTest(false, "");
		createSWTRowLayoutOneButton("");

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
		createWazaabiRowLayoutAndRemoveButtonTest(true, "");
		createSWTRowLayoutOneButton("");

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	@Test
	public void removeButtonAfterSetContentsWithRowData() {
		createWazaabiRowLayoutAndRemoveButtonTest(false, "RowData");
		createSWTRowLayoutOneButton("RowData");

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
		createWazaabiRowLayoutAndRemoveButtonTest(true, "RowData");
		createSWTRowLayoutOneButton("RowData");

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	
	@Test
	public void testModelSetMultipleRowLayoutBeforeSetContents () {
		testModelSetMultipleLayout(true);
		createSWTRowLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelSetMultipleRowLayoutAfterSetContents () {
		testModelSetMultipleLayout(false);
		createSWTRowLayoutTwoButtons("");
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelMoveRowLayoutBeforeSetContents () {
		testModelMoveLayout(true);
		createSWTRowLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelMoveRowLayoutAfterSetContents () {
		testModelMoveLayout(false);
		createSWTRowLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveRowLayoutByRemoveBeforeSetContents () {
		testModelRemoveLayoutByRemove(true);
		createSWTRowLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveRowLayoutByRemoveAfterSetContents () {
		testModelRemoveLayoutByRemove(false);
		createSWTRowLayoutTwoButtons("");
		getViewer().getUpdateManager().performUpdate();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveRowLayoutByRenameBeforeSetContents () {
		testModelRemoveLayoutByRename(true);
		createSWTRowLayoutTwoButtons("");
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveRowLayoutByRenameAfterSetContents () {
		testModelRemoveLayoutByRename(false);
		createSWTRowLayoutTwoButtons("");
		getViewer().getUpdateManager().performUpdate();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

}
