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

public class TestIntRuleMaximumSlider extends AbstractTestIntRuleMaximum{
	
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
	public void testModelSetMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelSetRule(true, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtSlider.getMaximum());
	}
	
	@Test
	public void testModelSetMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		Slider swtSlider = (Slider) ModelSetRule(false, widget, intRuleMaximum);
		Assert.assertEquals(MAX, swtSlider.getMaximum());
	}
	
	
	@Test
	public void testModelSetMultipleMaximumBeforeViewerSetContentsEqualsSWTMaximum() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(true, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtSlider.getMaximum());
	}
	
	@Test
	public void testModelSetMultipleMaximumAfterViewerSetContentsEqualsSWTMaximum() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(false, widget, intRuleMaximum, intRuleMaximum2);
		Assert.assertEquals(MAX, swtSlider.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumBeforeViewerSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelMoveRule(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}
	
	@Test
	public void testModelMoveMaximumAfterViewerSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelMoveRule(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveBeforeViewerSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRemoveAfterViewerSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}

	
	@Test 
	public void testModelRemoveMaximumByRenameBeforeViewerSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(true, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}
	
	@Test 
	public void testModelRemoveMaximumByRenameAfterViewerSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(false, widget, intRuleMaximum, intRuleMaximum2, "maximum");
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}
	
	
	@Test
	public void testModelModifyMaximumBeforeSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelModifyRule(true, widget);
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}
	
	@Test
	public void testModelModifyMaximumAfterSetContentsEqualsSWTMaximum(){
		Slider swtSlider = (Slider) ModelModifyRule(false, widget);
		Assert.assertEquals(MAX2, swtSlider.getMaximum());
	}

}
