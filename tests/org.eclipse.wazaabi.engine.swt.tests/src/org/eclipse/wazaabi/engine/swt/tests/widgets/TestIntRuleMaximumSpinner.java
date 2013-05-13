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

public class TestIntRuleMaximumSpinner extends AbstractTestIntRuleMaximum{
	
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
	public void testModelSetMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelSetRule(true, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtSpinner.getMaximum());
	}
	
	@Test
	public void testModelSetMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		Spinner swtSpinner = (Spinner) ModelSetRule(false, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtSpinner.getMaximum());
	}
	
	
	@Test
	public void testModelSetMultipleMaximumBeforeViewerSetContentsEqualsSWTMaximum() {
		Spinner swtSpinner = (Spinner) ModelSetMultipleRule(true, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtSpinner.getMaximum());
	}
	
	@Test
	public void testModelSetMultipleMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		Spinner swtSpinner = (Spinner) ModelSetMultipleRule(false, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtSpinner.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelMoveRule(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumAfterViewerSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelMoveRule(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveBeforeViewerSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRemove(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveAfterViewerSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRemove(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}

	
	@Test 
	public void testModelRemoveMaximumByRenameBeforeViewerSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRename(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRenameAfterViewerSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelRemoveRuleByRename(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}
	
	
	@Test
	public void testModelModifyMaximumBeforeSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelModifyRule(true, widget);
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}
	
	@Test
	public void testModelModifyMaximumAfterSetContentsEqualsSWTMaximum(){
		Spinner swtSpinner = (Spinner) ModelModifyRule(false, widget);
		Assert.assertEquals(MAX2, swtSpinner.getMaximum());
	}

}
