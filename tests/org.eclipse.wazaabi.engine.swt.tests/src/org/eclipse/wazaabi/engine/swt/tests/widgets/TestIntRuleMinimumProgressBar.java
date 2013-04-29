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

import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestIntRuleMinimumProgressBar extends AbstractTestIntRuleMinimum{
	
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
	public void testModelSetMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelSetRule(true, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtProgressBar.getMinimum());
	}
	
	@Test
	public void testModelSetMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetRule(false, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtProgressBar.getMinimum());
	}
	
	
	@Test
	public void testModelSetMultipleMinimumBeforeViewerSetContentsEqualsSWTMinimum() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetMultipleRule(true, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtProgressBar.getMinimum());
	}
	
	@Test
	public void testModelSetMultipleMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetMultipleRule(false, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtProgressBar.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelMoveRule(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumAfterViewerSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelMoveRule(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveBeforeViewerSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRemove(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveAfterViewerSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRemove(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}

	
	@Test 
	public void testModelRemoveMinimumByRenameBeforeViewerSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRename(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRenameAfterViewerSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRename(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}
	
	
	@Test
	public void testModelModifyMinimumBeforeSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelModifyRule(true, widget);
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}
	
	@Test
	public void testModelModifyMinimumAfterSetContentsEqualsSWTMinimum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelModifyRule(false, widget);
		Assert.assertEquals(MIN2, swtProgressBar.getMinimum());
	}

}
