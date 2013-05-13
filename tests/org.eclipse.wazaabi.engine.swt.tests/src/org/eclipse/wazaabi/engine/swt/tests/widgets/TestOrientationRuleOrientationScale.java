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
import org.eclipse.swt.widgets.Scale;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestOrientationRuleOrientationScale extends AbstractTestOrientationRuleOrientation{
	
	private org.eclipse.wazaabi.mm.core.widgets.Scale widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createScale();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	
	@Test
	public void testModelSetOrientationBeforeViewerSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelSetRule(true, widget, orientationRuleOrientation);
		Assert.assertEquals(SWT.HORIZONTAL, swtScale.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelSetOrientationAfterViewerSetContentsEqualsSWTOrientation() {
		Scale swtScale = (Scale) ModelSetRule(false, widget, orientationRuleOrientation);
		Assert.assertEquals(SWT.HORIZONTAL, swtScale.getStyle()&SWT.HORIZONTAL);
	}
	
	
	@Test
	public void testModelSetMultipleOrientationBeforeViewerSetContentsEqualsSWTOrientation() {
		Scale swtScale = (Scale) ModelSetMultipleRule(true, widget, orientationRuleOrientation, orientationRuleOrientation2);
		Assert.assertEquals(SWT.HORIZONTAL, swtScale.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelSetMultipleOrientationAfterViewerSetContentsEqualsSWTOrientation() {
		Scale swtScale = (Scale) ModelSetMultipleRule(false, widget, orientationRuleOrientation, orientationRuleOrientation2);
		Assert.assertEquals(SWT.HORIZONTAL, swtScale.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelMoveOrientationBeforeViewerSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelMoveRule(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}
	
	@Test
	public void testModelMoveOrientationAfterViewerSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelMoveRule(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRemoveBeforeViewerSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRemoveAfterViewerSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}

	
	@Test 
	public void testModelRemoveOrientationByRenameBeforeViewerSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRenameAfterViewerSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}
	
	
	@Test
	public void testModelModifyOrientationBeforeSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelModifyRule(true, widget);
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}
	
	@Test
	public void testModelModifyOrientationAfterSetContentsEqualsSWTOrientation(){
		Scale swtScale = (Scale) ModelModifyRule(false, widget);
		Assert.assertEquals(SWT.VERTICAL, swtScale.getStyle()&SWT.VERTICAL);
	}

}
