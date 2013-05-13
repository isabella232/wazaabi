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

public class TestLayoutInALayout extends AbstractTestLayoutInALayout {


	
	@Test
	public void testRowLayoutInARowLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "RowLayout";
		String childLayout = "RowLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

//		while (!getMainShell().isDisposed()) {
//			if (!getDisplay().readAndDispatch())
//				getDisplay().sleep();
//		}
		
//		Assert.assertTrue(SWTUtils.compareHierarchy(getMainShell(),swtShell));

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testRowLayoutInARowLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "RowLayout";
		String childLayout = "RowLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testGridLayoutInARowLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "RowLayout";
		String childLayout = "GridLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testGridLayoutInARowLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "RowLayout";
		String childLayout = "GridLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);
		getViewer().getUpdateManager().performUpdate();

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testFillLayoutInARowLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "RowLayout";
		String childLayout = "FillLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testFillLayoutInARowLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "RowLayout";
		String childLayout = "FillLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testGridLayoutInAGridLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "GridLayout";
		String childLayout = "GridLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testGridLayoutInAGridLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "GridLayout";
		String childLayout = "GridLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testRowLayoutInAGridLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "GridLayout";
		String childLayout = "RowLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testRowLayoutInAGridLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "GridLayout";
		String childLayout = "RowLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testFillLayoutInAGridLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "GridLayout";
		String childLayout = "FillLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testFillLayoutInAGridLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "GridLayout";
		String childLayout = "FillLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testFillLayoutInAFillLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "FillLayout";
		String childLayout = "FillLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testFillLayoutInAFillLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "FillLayout";
		String childLayout = "FillLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testRowLayoutInAFillLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "FillLayout";
		String childLayout = "RowLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);
		
//		while (!getMainShell().isDisposed()) {
//			if (!getDisplay().readAndDispatch())
//				getDisplay().sleep();
//		}

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testRowLayoutInAFillLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "FillLayout";
		String childLayout = "RowLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testGridLayoutInAFillLayoutWithNoLayoutDataBeforeViewerSetContents() {
		String parentLayout = "FillLayout";
		String childLayout = "GridLayout";
		boolean before = true;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
	@Test
	public void testGridLayoutInAFillLayoutWithNoLayoutDataAfterViewerSetContents() {
		String parentLayout = "FillLayout";
		String childLayout = "GridLayout";
		boolean before = false;
		
		createWazaabiWidget(before, parentLayout, childLayout);
		createSWTWidget(parentLayout, childLayout);

		getViewer().getUpdateManager().performUpdate();
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), label, swtLabel));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), button2, swtButton2));
	}
	
//	@Test
//	public void testBoundsOfModelButtonWithNoRowDataAddedAfterViewerSetContents() {
//		addButtonsTest(false);
//	}


}
