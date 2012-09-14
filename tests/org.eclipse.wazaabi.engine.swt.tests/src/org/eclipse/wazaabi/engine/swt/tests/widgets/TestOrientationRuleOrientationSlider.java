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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestOrientationRuleOrientationSlider extends AbstractTestOrientationRuleOrientation{
	
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
	public void testModelSetOrientationBeforeViewerSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelSetRule(true, widget, orientationRuleOrientation);
		Assert.assertEquals(SWT.HORIZONTAL, swtSlider.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelSetOrientationAfterViewerSetContentsEqualsSWTOrientation() {
		Slider swtSlider = (Slider) ModelSetRule(false, widget, orientationRuleOrientation);
		Assert.assertEquals(SWT.HORIZONTAL, swtSlider.getStyle()&SWT.HORIZONTAL);
	}
	
	
	@Test
	public void testModelSetMultipleOrientationBeforeViewerSetContentsEqualsSWTOrientation() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(true, widget, orientationRuleOrientation, orientationRuleOrientation2);
		Assert.assertEquals(SWT.HORIZONTAL, swtSlider.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelSetMultipleOrientationAfterViewerSetContentsEqualsSWTOrientation() {
		Slider swtSlider = (Slider) ModelSetMultipleRule(false, widget, orientationRuleOrientation, orientationRuleOrientation2);
		Assert.assertEquals(SWT.HORIZONTAL, swtSlider.getStyle()&SWT.HORIZONTAL);
	}
	
	@Test
	public void testModelMoveOrientationBeforeViewerSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelMoveRule(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}
	
	@Test
	public void testModelMoveOrientationAfterViewerSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelMoveRule(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRemoveBeforeViewerSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRemoveAfterViewerSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRemove(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}

	
	@Test 
	public void testModelRemoveOrientationByRenameBeforeViewerSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(true, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}
	
	@Test 
	public void testModelRemoveOrientationByRenameAfterViewerSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelRemoveRuleByRename(false, widget, orientationRuleOrientation, orientationRuleOrientation2, "orientation");
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}
	
	
	@Test
	public void testModelModifyOrientationBeforeSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelModifyRule(true, widget);
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}
	
	@Test
	public void testModelModifyOrientationAfterSetContentsEqualsSWTOrientation(){
		Slider swtSlider = (Slider) ModelModifyRule(false, widget);
		Assert.assertEquals(SWT.VERTICAL, swtSlider.getStyle()&SWT.VERTICAL);
	}

}
