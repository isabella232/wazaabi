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

public class TestIntRuleMaximumScale extends AbstractTestIntRuleMaximum{
	
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
	public void testModelSetMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelSetRule(true, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtScale.getMaximum());
	}
	
	@Test
	public void testModelSetMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		Scale swtScale = (Scale) ModelSetRule(false, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtScale.getMaximum());
	}
	
	
	@Test
	public void testModelSetMultipleMaximumBeforeViewerSetContentsEqualsSWTMaximum() {
		Scale swtScale = (Scale) ModelSetMultipleRule(true, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtScale.getMaximum());
	}
	
	@Test
	public void testModelSetMultipleMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		Scale swtScale = (Scale) ModelSetMultipleRule(false, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtScale.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelMoveRule(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumAfterViewerSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelMoveRule(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveBeforeViewerSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveAfterViewerSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRemove(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}

	
	@Test 
	public void testModelRemoveMaximumByRenameBeforeViewerSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRenameAfterViewerSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelRemoveRuleByRename(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}
	
	
	@Test
	public void testModelModifyMaximumBeforeSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelModifyRule(true, widget);
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}
	
	@Test
	public void testModelModifyMaximumAfterSetContentsEqualsSWTMaximum(){
		Scale swtScale = (Scale) ModelModifyRule(false, widget);
		Assert.assertEquals(MAX2, swtScale.getMaximum());
	}

}
