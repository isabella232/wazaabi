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

public class TestIntRuleIncrementScale extends AbstractTestIntRuleIncrement{
	
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
	public void testModelSetIncrementBeforeViewerSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelSetRule(true, widget, intRuleIncrement);
		Assert.assertEquals(INC, swtScale.getIncrement());
	}
	
	@Test
	public void testModelSetIncrementAfterViewerSetContentsEqualsSWTIncrement() {
		Scale swtScale = (Scale) ModelSetRule(false, widget, intRuleIncrement);
		Assert.assertEquals(INC, swtScale.getIncrement());
	}
	
	
	@Test
	public void testModelSetMultipleIncrementBeforeViewerSetContentsEqualsSWTIncrement() {
		Scale swtScale = (Scale) ModelSetMultipleRule(true, widget, intRuleIncrement, intRuleIncrement2);
		Assert.assertEquals(INC, swtScale.getIncrement());
	}
	
	@Test
	public void testModelSetMultipleIncrementAfterViewerSetContentsEqualsSWTIncrement() {
		Scale swtScale = (Scale) ModelSetMultipleRule(false, widget, intRuleIncrement, intRuleIncrement2);
		Assert.assertEquals(INC, swtScale.getIncrement());
	}
	
	@Test
	public void testModelMoveIncrementBeforeViewerSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelMoveRule(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}
	
	@Test
	public void testModelMoveIncrementAfterViewerSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelMoveRule(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRemoveBeforeViewerSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRemoveAfterViewerSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}

	
	@Test 
	public void testModelRemoveIncrementByRenameBeforeViewerSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRenameAfterViewerSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}
	
	
	@Test
	public void testModelModifyIncrementBeforeSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelModifyRule(true, widget);
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}
	
	@Test
	public void testModelModifyIncrementAfterSetContentsEqualsSWTIncrement(){
		Scale swtScale = (Scale) ModelModifyRule(false, widget);
		Assert.assertEquals(INC2, swtScale.getIncrement());
	}

}
