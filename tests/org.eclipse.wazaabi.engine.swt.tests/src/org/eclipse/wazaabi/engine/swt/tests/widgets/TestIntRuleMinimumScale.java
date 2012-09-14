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

public class TestIntRuleMinimumScale extends AbstractTestIntRuleMinimum{
	
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
	public void testModelSetMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelSetRule(true, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtScale.getMinimum());
	}
	
	@Test
	public void testModelSetMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		Scale swtScale = (Scale) ModelSetRule(false, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtScale.getMinimum());
	}
	
	
	@Test
	public void testModelSetMultipleMinimumBeforeViewerSetContentsEqualsSWTMinimum() {
		Scale swtScale = (Scale) ModelSetMultipleRule(true, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtScale.getMinimum());
	}
	
	@Test
	public void testModelSetMultipleMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		Scale swtScale = (Scale) ModelSetMultipleRule(false, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtScale.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelMoveRule(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumAfterViewerSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelMoveRule(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveBeforeViewerSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveAfterViewerSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}

	
	@Test 
	public void testModelRemoveMinimumByRenameBeforeViewerSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRenameAfterViewerSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}
	
	
	@Test
	public void testModelModifyMinimumBeforeSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelModifyRule(true, widget);
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}
	
	@Test
	public void testModelModifyMinimumAfterSetContentsEqualsSWTMinimum(){
		Scale swtScale = (Scale) ModelModifyRule(false, widget);
		Assert.assertEquals(MIN2, swtScale.getMinimum());
	}

}
