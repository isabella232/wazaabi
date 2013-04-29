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

public class TestIntRuleIncrementSpinner extends AbstractTestIntRuleIncrement{
	
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
	public void testModelSetIncrementBeforeViewerSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelSetRule(true, widget, intRuleIncrement);
		Assert.assertEquals(INC, swtSpinner.getIncrement());
	}
	
	@Test
	public void testModelSetIncrementAfterViewerSetContentsEqualsSWTIncrement() {
		Spinner swtSpinner = (Spinner) ModelSetRule(false, widget, intRuleIncrement);
		Assert.assertEquals(INC, swtSpinner.getIncrement());
	}
	
	
	@Test
	public void testModelSetMultipleIncrementBeforeViewerSetContentsEqualsSWTIncrement() {
		Spinner swtSpinner = (Spinner) ModelSetMultipleRule(true, widget, intRuleIncrement, intRuleIncrement2);
		Assert.assertEquals(INC, swtSpinner.getIncrement());
	}
	
	@Test
	public void testModelSetMultipleIncrementAfterViewerSetContentsEqualsSWTIncrement() {
		Spinner swtSpinner = (Spinner) ModelSetMultipleRule(false, widget, intRuleIncrement, intRuleIncrement2);
		Assert.assertEquals(INC, swtSpinner.getIncrement());
	}
	
	@Test
	public void testModelMoveIncrementBeforeViewerSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelMoveRule(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}
	
	@Test
	public void testModelMoveIncrementAfterViewerSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelMoveRule(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRemoveBeforeViewerSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRemove(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRemoveAfterViewerSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRemove(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}

	
	@Test 
	public void testModelRemoveIncrementByRenameBeforeViewerSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRename(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRenameAfterViewerSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRename(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}
	
	
	@Test
	public void testModelModifyIncrementBeforeSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelModifyRule(true, widget);
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}
	
	@Test
	public void testModelModifyIncrementAfterSetContentsEqualsSWTIncrement(){
		Spinner swtSpinner = (Spinner) ModelModifyRule(false, widget);
		Assert.assertEquals(INC2, swtSpinner.getIncrement());
	}

}
