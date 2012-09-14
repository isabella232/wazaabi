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

public class TestIntRulePageIncrementSlider extends AbstractTestIntRulePageIncrement{
	
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
	public void testModelSetPageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelSetRule(true, widget, intRulePageIncrement);
		Assert.assertEquals(PINC, swtSlider.getPageIncrement());
	}
	
	@Test
	public void testModelSetPageIncrementAfterViewerSetContentsEqualsSWTPageIncrement() {
		Slider swtSlider = (Slider) ModelSetRule(false, widget, intRulePageIncrement);
		Assert.assertEquals(PINC, swtSlider.getPageIncrement());
	}
	
	
	@Test
	public void testModelSetMultiplePageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(true, widget, intRulePageIncrement, intRulePageIncrement2);
		Assert.assertEquals(PINC, swtSlider.getPageIncrement());
	}
	
	@Test
	public void testModelSetMultiplePageIncrementAfterViewerSetContentsEqualsSWTPageIncrement() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(false, widget, intRulePageIncrement, intRulePageIncrement2);
		Assert.assertEquals(PINC, swtSlider.getPageIncrement());
	}
	
	@Test
	public void testModelMovePageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelMoveRule(true, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}
	
	@Test
	public void testModelMovePageIncrementAfterViewerSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelMoveRule(false, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}
	
	@Test 
	public void testModelRemovePageIncrementByRemoveBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(true, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}
	
	@Test 
	public void testModelRemovePageIncrementByRemoveAfterViewerSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(false, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}

	
	@Test 
	public void testModelRemovePageIncrementByRenameBeforeViewerSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(true, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}
	
	@Test 
	public void testModelRemovePageIncrementByRenameAfterViewerSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(false, widget, intRulePageIncrement, intRulePageIncrement2, "pageIncrement");
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}
	
	
	@Test
	public void testModelModifyPageIncrementBeforeSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelModifyRule(true, widget);
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}
	
	@Test
	public void testModelModifyPageIncrementAfterSetContentsEqualsSWTPageIncrement(){
		Slider swtSlider = (Slider) ModelModifyRule(false, widget);
		Assert.assertEquals(PINC2, swtSlider.getPageIncrement());
	}

}
