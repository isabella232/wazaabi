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

import org.eclipse.swt.widgets.Slider;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestIntRuleMinimumSlider extends AbstractTestIntRuleMinimum{
	
	private org.eclipse.wazaabi.mm.core.widgets.Slider widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createSlider();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	
	@Test
	public void testModelSetMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelSetRule(true, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtSlider.getMinimum());
	}
	
	@Test
	public void testModelSetMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		Slider swtSlider = (Slider) ModelSetRule(false, widget, intRuleMinimum);
		Assert.assertEquals(MIN, swtSlider.getMinimum());
	}
	
	
	@Test
	public void testModelSetMultipleMinimumBeforeViewerSetContentsEqualsSWTMinimum() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(true, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtSlider.getMinimum());
	}
	
	@Test
	public void testModelSetMultipleMinimumAfterViewerSetContentsEqualsSWTMinimum() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(false, widget, intRuleMinimum, intRuleMinimum2);
		Assert.assertEquals(MIN, swtSlider.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumBeforeViewerSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelMoveRule(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}
	
	@Test
	public void testModelMoveMinimumAfterViewerSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelMoveRule(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveBeforeViewerSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRemoveAfterViewerSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}

	
	@Test 
	public void testModelRemoveMinimumByRenameBeforeViewerSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(true, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}
	
	@Test 
	public void testModelRemoveMinimumByRenameAfterViewerSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(false, widget, intRuleMinimum, intRuleMinimum2, "minimum");
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}
	
	
	@Test
	public void testModelModifyMinimumBeforeSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelModifyRule(true, widget);
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}
	
	@Test
	public void testModelModifyMinimumAfterSetContentsEqualsSWTMinimum(){
		Slider swtSlider = (Slider) ModelModifyRule(false, widget);
		Assert.assertEquals(MIN2, swtSlider.getMinimum());
	}

}
