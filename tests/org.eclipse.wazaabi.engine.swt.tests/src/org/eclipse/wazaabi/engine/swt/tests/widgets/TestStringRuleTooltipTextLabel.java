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

import org.eclipse.swt.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestStringRuleTooltipTextLabel extends AbstractTestStringRuleTooltipText{
	
	private org.eclipse.wazaabi.mm.core.widgets.Label widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createLabel();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	@Test
	public void testModelSetTooltipTextBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelSetRule(true, widget, stringRuleTooltipText);
		Assert.assertEquals(TOOLTIPTEXT, swtLabel.getToolTipText());
	}
	
	@Test
	public void testModelSetTooltipTextAfterViewerSetContentsEqualsSWTText() {
		Label swtLabel = (Label) ModelSetRule(false, widget, stringRuleTooltipText);
		Assert.assertEquals(TOOLTIPTEXT, swtLabel.getToolTipText());
	}
	
	@Test
	public void testModelSetMultipleTooltipTextBeforeViewerSetContentsEqualsSWTText() {
		Label swtLabel = (Label) ModelSetMultipleRule(true, widget, stringRuleTooltipText, stringRuleTooltipText2);
		Assert.assertEquals(TOOLTIPTEXT, swtLabel.getToolTipText());
	}
	
	@Test
	public void testModelSetMultipleTooltipTextAfterViewerSetContentsEqualsSWTText() {
		Label swtLabel = (Label) ModelSetMultipleRule(false, widget, stringRuleTooltipText, stringRuleTooltipText2);
		Assert.assertEquals(TOOLTIPTEXT, swtLabel.getToolTipText());
	}

	
	@Test
	public void testModelMoveTooltipTextBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelMoveRule(true, widget, stringRuleTooltipText, stringRuleTooltipText2, "tooltip-text");
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}
	
	@Test
	public void testModelMoveTooltipTextAfterViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelMoveRule(false, widget, stringRuleTooltipText, stringRuleTooltipText2, "tooltip-text");
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}
	
	@Test 
	public void testModelRemoveTooltipTextByRemoveBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(true, widget, stringRuleTooltipText, stringRuleTooltipText2, "tooltip-text");
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}
	
	@Test 
	public void testModelRemoveTooltipTextByRemoveAfterViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(false, widget, stringRuleTooltipText, stringRuleTooltipText2, "tooltip-text");
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}
	
	@Test 
	public void testModelRemoveTooltipTextByRenameBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(true, widget, stringRuleTooltipText, stringRuleTooltipText2, "tooltip-text");
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}
	
	@Test 
	public void testModelRemoveTooltipTextByRenameAfterViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(false, widget, stringRuleTooltipText, stringRuleTooltipText2, "tooltip-text");
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}
	
	
	@Test
	public void testModelModifyTooltipTextBeforeSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelModifyRule(true, widget);
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}
	
	@Test
	public void testModelModifyTooltipTextAfterSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelModifyRule(false, widget);
		Assert.assertEquals(TOOLTIPTEXT2, swtLabel.getToolTipText());
	}

}
