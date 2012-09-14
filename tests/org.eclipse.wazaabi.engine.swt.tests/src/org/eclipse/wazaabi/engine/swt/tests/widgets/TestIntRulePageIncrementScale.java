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

import org.eclipse.swt.widgets.Scale;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestIntRulePageIncrementScale extends AbstractTestIntRulePageIncrement{
	
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
	public void testModelSetPageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelSetRule(true, widget, intRulePageIncrement);
		Assert.assertEquals(PINC, swtScale.getPageIncrement());
	}
	
	@Test
	public void testModelSetPageIncrementAfterViewerSetContentsEqualsSWTPageIncrement() {
		Scale swtScale = (Scale) ModelSetRule(false, widget, intRulePageIncrement);
		Assert.assertEquals(PINC, swtScale.getPageIncrement());
	}
	
	
	@Test
	public void testModelSetMultiplePageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement() {
		Scale swtScale = (Scale) ModelSetMultipleRule(true, widget, intRulePageIncrement, intRulePageIncrement2);
		Assert.assertEquals(PINC, swtScale.getPageIncrement());
	}
	
	@Test
	public void testModelSetMultiplePageIncrementAfterViewerSetContentsEqualsSWTPageIncrement() {
		Scale swtScale = (Scale) ModelSetMultipleRule(false, widget, intRulePageIncrement, intRulePageIncrement2);
		Assert.assertEquals(PINC, swtScale.getPageIncrement());
	}
	
	@Test
	public void testModelMovePageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelMoveRule(true, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}
	
	@Test
	public void testModelMovePageIncrementAfterViewerSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelMoveRule(false, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}
	
	@Test 
	public void testModelRemovePageIncrementByRemoveBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(true, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}
	
	@Test 
	public void testModelRemovePageIncrementByRemoveAfterViewerSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(false, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}

	
	@Test 
	public void testModelRemovePageIncrementByRenameBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(true, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}
	
	@Test 
	public void testModelRemovePageIncrementByRenameAfterViewerSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(false, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}
	
	
	@Test
	public void testModelModifyPageIncrementBeforeSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelModifyRule(true, widget);
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}
	
	@Test
	public void testModelModifyPageIncrementAfterSetContentsEqualsSWTPageIncrement(){
		Scale swtScale = (Scale) ModelModifyRule(false, widget);
		Assert.assertEquals(PINC2, swtScale.getPageIncrement());
	}

}
