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

import org.eclipse.swt.widgets.Spinner;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestIntRuleMinimumSpinner extends AbstractTestIntRuleMinimum{
	
	private org.eclipse.wazaabi.mm.core.widgets.Spinner widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createSpinner();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	
	@Test
	public void testModelSetMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelSetRule(true, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtSpinner.getMinimum());
	}
	
	@Test
	public void testModelSetMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		Spinner swtSpinner = (Spinner) ModelSetRule(false, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtSpinner.getMinimum());
	}
	
	
	@Test
	public void testModelSetMultipleMinimumBeforeViewerSetContentsEqualsSWTMinimum() {
		Spinner swtSpinner = (Spinner) ModelSetMultipleRule(true, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtSpinner.getMinimum());
	}
	
	@Test
	public void testModelSetMultipleMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		Spinner swtSpinner = (Spinner) ModelSetMultipleRule(false, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtSpinner.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelMoveRule(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumAfterViewerSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelMoveRule(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveBeforeViewerSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRemove(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveAfterViewerSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRemove(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}

	
	@Test 
	public void testModelRemoveMinimumByRenameBeforeViewerSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRename(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRenameAfterViewerSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRename(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}
	
	
	@Test
	public void testModelModifyMinimumBeforeSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelModifyRule(true, widget);
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}
	
	@Test
	public void testModelModifyMinimumAfterSetContentsEqualsSWTMinimum(){
		Spinner swtSpinner = (Spinner) ModelModifyRule(false, widget);
		Assert.assertEquals(MIN2, swtSpinner.getMinimum());
	}

}
