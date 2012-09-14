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
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.junit.Assert;
import org.junit.Test;

public class TestSashForm extends AbstractTestSashForm {



//	@Test
//	public void showWazaabiWidget() {
//		createWazaabiSashForm(true, Orientation.VERTICAL);
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();
//	}
//	
//	@Test
//	public void showSWTWidget() {
//		createSWTSashForm(SWT.VERTICAL);
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
//		display.dispose();
//	}

	
	@Test
	public void testModelSashFormTextSetBeforeViewerSetContentsEqualsText() {

		createWazaabiSashForm(true, Orientation.VERTICAL);

		org.eclipse.swt.widgets.Button wazEl1 = (org.eclipse.swt.widgets.Button) SWTUtils.getWidget(viewer, wazButton1);
		Assert.assertEquals(TEXT1, wazEl1.getText());
		
		org.eclipse.swt.widgets.Button wazEl2 = (org.eclipse.swt.widgets.Button) SWTUtils.getWidget(viewer, wazButton2);
		Assert.assertEquals(TEXT2, wazEl2.getText());
		
		org.eclipse.swt.widgets.Label wazEl3 = (org.eclipse.swt.widgets.Label) SWTUtils.getWidget(viewer, wazLabel);
		Assert.assertEquals(TEXT3, wazEl3.getText());
	}
	
	@Test
	public void testModelSashFormTextSetAfterViewerSetContentsEqualsText() {

		createWazaabiSashForm(false, Orientation.VERTICAL);

		org.eclipse.swt.widgets.Button wazEl = (org.eclipse.swt.widgets.Button) SWTUtils.getWidget(viewer, wazButton1);
		Assert.assertEquals(TEXT1, wazEl.getText());
		
		org.eclipse.swt.widgets.Button wazEl2 = (org.eclipse.swt.widgets.Button) SWTUtils.getWidget(viewer, wazButton2);
		Assert.assertEquals(TEXT2, wazEl2.getText());
		
		org.eclipse.swt.widgets.Label wazEl3 = (org.eclipse.swt.widgets.Label) SWTUtils.getWidget(viewer, wazLabel);
		Assert.assertEquals(TEXT3, wazEl3.getText());
	}
	
	@Test
	public void testModelSashFormSizeVerticalBeforeViewerSetContents() {
		
		createWazaabiSashForm(true, Orientation.VERTICAL);
		createSWTSashForm(SWT.VERTICAL);
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton2, swtButton2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazLabel, swtLabel));
	}
	
	@Test
	public void testModelSashFormSizeVerticalAfterViewerSetContents() {
		
		createWazaabiSashForm(false, Orientation.VERTICAL);
		createSWTSashForm(SWT.VERTICAL);
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton2, swtButton2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazLabel, swtLabel));
	}
	
	@Test
	public void testModelSashFormSizeHorizontalBeforeViewerSetContents() {
		
		createWazaabiSashForm(true, Orientation.HORIZONTAL);
		createSWTSashForm(SWT.HORIZONTAL);
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton2, swtButton2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazLabel, swtLabel));
	}
	
	@Test
	public void testModelSashFormSizeHorizontalAfterViewerSetContents() {
		
		createWazaabiSashForm(false, Orientation.HORIZONTAL);
		createSWTSashForm(SWT.HORIZONTAL);
		
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton1, swtButton1));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazButton2, swtButton2));
		Assert.assertTrue(SWTUtils.haveSameBounds(getViewer(), wazLabel, swtLabel));
	}

}
