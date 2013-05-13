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

public class TestIntRuleMaximumProgressBar extends AbstractTestIntRuleMaximum{
	
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
	public void testModelSetMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelSetRule(true, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtProgressBar.getMaximum());
	}
	
	@Test
	public void testModelSetMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetRule(false, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtProgressBar.getMaximum());
	}
	
	
	@Test
	public void testModelSetMultipleMaximumBeforeViewerSetContentsEqualsSWTMaximum() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetMultipleRule(true, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtProgressBar.getMaximum());
	}
	
	@Test
	public void testModelSetMultipleMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		ProgressBar swtProgressBar = (ProgressBar) ModelSetMultipleRule(false, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtProgressBar.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelMoveRule(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumAfterViewerSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelMoveRule(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveBeforeViewerSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRemove(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveAfterViewerSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRemove(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}

	
	@Test 
	public void testModelRemoveMaximumByRenameBeforeViewerSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRename(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRenameAfterViewerSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelRemoveRuleByRename(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}
	
	
	@Test
	public void testModelModifyMaximumBeforeSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelModifyRule(true, widget);
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}
	
	@Test
	public void testModelModifyMaximumAfterSetContentsEqualsSWTMaximum(){
		ProgressBar swtProgressBar = (ProgressBar) ModelModifyRule(false, widget);
		Assert.assertEquals(MAX2, swtProgressBar.getMaximum());
	}

}
