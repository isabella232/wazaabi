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
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestOrientationRuleOrientationProgressBar extends AbstractTestOrientationRuleOrientation{
	
	private org.eclipse.wazaabi.mm.core.widgets.ProgressBar widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createProgressBar();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	
	@Test
	public void testModelSetOrientationBeforeViewerSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelSetRule(true, widget, orientationRuleOrientation);
		Assert.assertEquals(SWT.HORIZONTAL, swtProgressBar.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelSetOrientationAfterViewerSetContentsEqualsSWTOrientation() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetRule(false, widget, orientationRuleOrientation);
		Assert.assertEquals(SWT.HORIZONTAL, swtProgressBar.getStyle()&SWT.HORIZONTAL);
	}
	
	
	@Test
	public void testModelSetMultipleOrientationBeforeViewerSetContentsEqualsSWTOrientation() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetMultipleRule(true, widget, orientationRuleOrientation, orientationRuleOrientation2);
		Assert.assertEquals(SWT.HORIZONTAL, swtProgressBar.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelSetMultipleOrientationAfterViewerSetContentsEqualsSWTOrientation() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetMultipleRule(false, widget, orientationRuleOrientation, orientationRuleOrientation2);
		Assert.assertEquals(SWT.HORIZONTAL, swtProgressBar.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelMoveOrientationBeforeViewerSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelMoveRule(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}
	
	@Test
	public void testModelMoveOrientationAfterViewerSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelMoveRule(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRemoveBeforeViewerSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRemove(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRemoveAfterViewerSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRemove(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}

	
	@Test 
	public void testModelRemoveOrientationByRenameBeforeViewerSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRename(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRenameAfterViewerSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRename(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}
	
	
	@Test
	public void testModelModifyOrientationBeforeSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelModifyRule(true, widget);
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}
	
	@Test
	public void testModelModifyOrientationAfterSetContentsEqualsSWTOrientation(){
		ProgressBar swtProgressBar = (ProgressBar) ModelModifyRule(false, widget);
		Assert.assertEquals(SWT.VERTICAL, swtProgressBar.getStyle()&SWT.VERTICAL);
	}

}
