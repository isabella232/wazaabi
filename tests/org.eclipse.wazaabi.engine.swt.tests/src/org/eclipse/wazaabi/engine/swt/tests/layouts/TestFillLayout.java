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

public class TestFillLayout extends AbstractTestFillLayout {
	
	@Test
	public void addButtonAfterSetContents() {
		createWazaabiFillLayout(false, "");
		createSWTFillLayoutTwoButtons();
		getViewer().getUpdateManager().performUpdate();
	
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
		//display.dispose();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

	@Test
	public void addButtonBeforeSetContents () {
		createWazaabiFillLayout(true, "");
		createSWTFillLayoutTwoButtons();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

	@Test
	public void removeButtonAfterSetContents() {
		createWazaabiFillLayoutAndRemoveButtonTest(false, "");
		createSWTFillLayoutOneButton();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
		//display.dispose();

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	@Test
	public void removeButtonBeforeSetContents() {
		createWazaabiFillLayoutAndRemoveButtonTest(true, "");
		createSWTFillLayoutOneButton();
		
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertNull(SWTUtils.getWidget(viewer,button2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
	}
	
	
	@Test
	public void testModelSetMultipleFillLayoutBeforeSetContents () {
		testModelSetMultipleLayout(true);
		createSWTFillLayoutTwoButtons();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelSetMultipleFillLayoutAfterSetContents () {
		testModelSetMultipleLayout(false);
		createSWTFillLayoutTwoButtons();
		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelMoveFillLayoutBeforeSetContents () {
		testModelMoveLayout(true);
		createSWTFillLayoutTwoButtons();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelMoveFillLayoutAfterSetContents () {
		testModelMoveLayout(false);
		createSWTFillLayoutTwoButtons();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveFillLayoutByRemoveBeforeSetContents () {
		testModelRemoveLayoutByRemove(true);
		createSWTFillLayoutTwoButtons();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveFillLayoutByRemoveAfterSetContents () {
		testModelRemoveLayoutByRemove(false);
		createSWTFillLayoutTwoButtons();
		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveFillLayoutByRenameBeforeSetContents () {
		testModelRemoveLayoutByRename(true);
		createSWTFillLayoutTwoButtons();
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testModelRemoveFillLayoutByRenameAfterSetContents () {
		testModelRemoveLayoutByRename(false);
		createSWTFillLayoutTwoButtons();
		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}

}
