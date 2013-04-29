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

public class TestIntRuleIncrementSlider extends AbstractTestIntRuleIncrement{
	
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
	public void testModelSetIncrementBeforeViewerSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelSetRule(true, widget, intRuleIncrement);
		Assert.assertEquals(INC, swtSlider.getIncrement());
	}
	
	@Test
	public void testModelSetIncrementAfterViewerSetContentsEqualsSWTIncrement() {
		Slider swtSlider = (Slider) ModelSetRule(false, widget, intRuleIncrement);
		Assert.assertEquals(INC, swtSlider.getIncrement());
	}
	
	
	@Test
	public void testModelSetMultipleIncrementBeforeViewerSetContentsEqualsSWTIncrement() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(true, widget, intRuleIncrement, intRuleIncrement2);
		Assert.assertEquals(INC, swtSlider.getIncrement());
	}
	
	@Test
	public void testModelSetMultipleIncrementAfterViewerSetContentsEqualsSWTIncrement() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(false, widget, intRuleIncrement, intRuleIncrement2);
		Assert.assertEquals(INC, swtSlider.getIncrement());
	}
	
	@Test
	public void testModelMoveIncrementBeforeViewerSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelMoveRule(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}
	
	@Test
	public void testModelMoveIncrementAfterViewerSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelMoveRule(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRemoveBeforeViewerSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRemoveAfterViewerSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}

	
	@Test 
	public void testModelRemoveIncrementByRenameBeforeViewerSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(true, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}
	
	@Test 
	public void testModelRemoveIncrementByRenameAfterViewerSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(false, widget, intRuleIncrement, intRuleIncrement2, "increment");
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}
	
	
	@Test
	public void testModelModifyIncrementBeforeSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelModifyRule(true, widget);
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}
	
	@Test
	public void testModelModifyIncrementAfterSetContentsEqualsSWTIncrement(){
		Slider swtSlider = (Slider) ModelModifyRule(false, widget);
		Assert.assertEquals(INC2, swtSlider.getIncrement());
	}

}
