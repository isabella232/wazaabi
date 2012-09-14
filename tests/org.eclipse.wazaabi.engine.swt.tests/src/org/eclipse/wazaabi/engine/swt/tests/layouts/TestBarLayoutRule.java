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
import org.eclipse.wazaabi.mm.core.Orientation;
import org.junit.Assert;
import org.junit.Test;

public class TestBarLayoutRule extends AbstractTestBarLayout {

	@Test
	public void addButtonInCoolBarAfterSetContents() {
		createWazaabiBarLayout(false, "coolbar", null);
		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInCoolBarBeforeSetContents () {
		createWazaabiBarLayout(true, "coolbar", null);
		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void addButtonInToolBarAfterSetContents() {
		createWazaabiBarLayout(false, "toolbar", null);
		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInToolBarBeforeSetContents () {
		createWazaabiBarLayout(true, "toolbar", null);
		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addWidgetsInCoolBarAfterSetContents() {
		createWaraabiBarLayoutWithDifferentWidgets(false, "coolbar");
		createSWTCoolBarLayoutWithDifferentWidgets();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), scale, swtScale));
	}


	@Test
	public void addWidgetsInCoolBarBeforeSetContents () {
		createWaraabiBarLayoutWithDifferentWidgets(true, "coolbar");
		createSWTCoolBarLayoutWithDifferentWidgets();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), scale, swtScale));
	}

	@Test
	public void addWidgetsInToolBarAfterSetContents() {
		createWaraabiBarLayoutWithDifferentWidgets(false, "toolbar");
		createSWTToolBarLayoutWithDifferentWidgets();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), scale, swtScale));
	}


	@Test
	public void addWidgetsInToolBarBeforeSetContents () {
		createWaraabiBarLayoutWithDifferentWidgets(true, "toolbar");
		createSWTToolBarLayoutWithDifferentWidgets();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), scale, swtScale));
	}






	@Test
	public void addButtonInHorizontalCoolBarAfterSetContents() {
		createWazaabiBarLayout(false, "coolbar", Orientation.HORIZONTAL);
		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInHorizontalCoolBarBeforeSetContents () {
		createWazaabiBarLayout(true, "coolbar", Orientation.HORIZONTAL);

		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInHorizontalToolBarAfterSetContents() {
		createWazaabiBarLayout(false, "toolbar", Orientation.HORIZONTAL);
		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInHorizontalToolBarBeforeSetContents () {
		createWazaabiBarLayout(true, "toolbar", Orientation.HORIZONTAL);
		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInVerticalCoolBarAfterSetContents() {
		createWazaabiBarLayout(false, "coolbar", Orientation.VERTICAL);
		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInVerticalCoolBarBeforeSetContents () {
		createWazaabiBarLayout(true, "coolbar", Orientation.VERTICAL);
		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}



	@Test
	public void addButtonInVerticalToolBarAfterSetContents() {
		createWazaabiBarLayout(false, "toolbar", Orientation.VERTICAL);
		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}


	@Test
	public void addButtonInVerticalToolBarBeforeSetContents () {
		createWazaabiBarLayout(true, "toolbar", Orientation.VERTICAL);
		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void ConvertCoolBarInToolBarBeforeSetContents () {
		convertBarLayout("toolbar");
		createWazaabiBarLayout(true, "coolbar", Orientation.HORIZONTAL);

		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void ConvertCoolBarInToolBarAfterSetContents () {
		createWazaabiBarLayout(true, "coolbar", Orientation.HORIZONTAL);
		convertBarLayout("toolbar");

		createSWTToolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void ConvertToolBarInCoolBarBeforeSetContents () {
		convertBarLayout("coolbar");
		createWazaabiBarLayout(true, "toolbar", Orientation.HORIZONTAL);

		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void ConvertToolBarInCoolBarAfterSetContents () {
		createWazaabiBarLayout(true, "toolbar", Orientation.HORIZONTAL);
		convertBarLayout("coolbar");

		createSWTCoolBar();
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	/**
	 * Testing manipulation of BarLayoutRule with isdraggable false
	 * * SetMultiple
	 * * Move rules
	 * * Remove by rename
	 * * Remove by remove
	 *
	 */
	@Test
	public void testModelSetMultipleToolBarLayoutBeforeSetContents () {
		testModelSetMultipleLayout(true, "toolbar");
		createSWTToolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelSetMultipleToolBarLayoutAfterSetContents () {
		testModelSetMultipleLayout(false, "toolbar");
		createSWTToolBar();

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelMoveToolBarLayoutBeforeSetContents () {
		testModelMoveLayout(true, "toolbar");
		createSWTToolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelMoveToolBarLayoutAfterSetContents () {
		testModelMoveLayout(false, "toolbar");
		createSWTToolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveToolBarLayoutByRemoveBeforeSetContents () {
		testModelRemoveLayoutByRemove(true, "toolbar");
		createSWTToolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveToolBarLayoutByRemoveAfterSetContents () {
		testModelRemoveLayoutByRemove(false, "toolbar");
		createSWTToolBar();

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveToolBarLayoutByRenameBeforeSetContents () {
		testModelRemoveLayoutByRename(true, "toolbar");
		createSWTToolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveToolBarLayoutByRenameAfterSetContents () {
		testModelRemoveLayoutByRename(false, "toolbar");
		createSWTToolBar();

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}



	/**
	 * Testing manipulation of BarLayoutRule with isdraggable true
	 * * SetMultiple
	 * * Move rules
	 * * Remove by rename
	 * * Remove by remove
	 */
	@Test
	public void testModelSetMultipleCoolBarLayoutBeforeSetContents () {
		testModelSetMultipleLayout(true, "coolbar");
		createSWTCoolBar();

//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelSetMultipleCoolBarLayoutAfterSetContents () {
		testModelSetMultipleLayout(false, "coolbar");
		createSWTCoolBar();

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelMoveCoolBarLayoutBeforeSetContents () {
		testModelMoveLayout(true, "coolbar");
		createSWTCoolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelMoveCoolBarLayoutAfterSetContents () {
		testModelMoveLayout(false, "coolbar");
		createSWTCoolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveCoolBarLayoutByRemoveBeforeSetContents () {
		testModelRemoveLayoutByRemove(true, "coolbar");
		createSWTCoolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveCoolBarLayoutByRemoveAfterSetContents () {
		testModelRemoveLayoutByRemove(false, "coolbar");
		createSWTCoolBar();

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveCoolBarLayoutByRenameBeforeSetContents () {
		testModelRemoveLayoutByRename(true, "coolbar");
		createSWTCoolBar();

		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}

	@Test
	public void testModelRemoveCoolBarLayoutByRenameAfterSetContents () {
		testModelRemoveLayoutByRename(false, "coolbar");
		createSWTCoolBar();

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameWidth(getViewer(), button2, swtButton2));
	}
}
