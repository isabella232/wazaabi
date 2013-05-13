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
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.HyperlinkRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestHyperlinkRuleLookAndFeelLabel extends AbstractTestStringRuleText{
	
	private org.eclipse.wazaabi.mm.core.widgets.Label widget;
	private HyperlinkRule hyperlinkRule;
	
	
	@Override
	public void before() {
		super.before();
		hyperlinkRule = CoreStylesFactory.eINSTANCE.createHyperlinkRule();
		hyperlinkRule.setPropertyName("lookandfeel");
		widget = CoreWidgetsFactory.eINSTANCE.createLabel();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	
	@Test
	public void testModelSetHyperlinkRuleBeforeViewerSetContentsEqualsSWTLink(){
		Widget swtLink = ModelSetRule(true, widget, hyperlinkRule);
		Assert.assertTrue(swtLink instanceof Link);
		
	}
	
	@Test
	public void testModelSetHyperlinkRuleAfterViewerSetContentsEqualsSWTLink(){
		Widget swtLink = ModelSetRule(false, widget, hyperlinkRule);
		Assert.assertTrue(swtLink instanceof Link);
		
	}
	
	@Test
	public void testModelRemoveHyperlinkRuleBeforeViewerSetContentsEqualsSWTLink(){
		Widget swtLink = ModelRemoveSingleRuleByRemove(true, widget, hyperlinkRule, "lookandfeel");
		Assert.assertTrue(swtLink instanceof Label);
		
	}
	
	@Test
	public void testModelRemoveHyperlinkRuleAfterViewerSetContentsEqualsSWTLink(){
		Widget swtLink = ModelRemoveSingleRuleByRemove(false, widget, hyperlinkRule, "lookandfeel");
		Assert.assertTrue(swtLink instanceof Label);
		
	}

	@Test
	public void testModelSetTextBeforeViewerSetContentsEqualsSWTText(){
		Link swtLink = (Link) ModelSetMultipleRule(true, widget, hyperlinkRule, stringRuleText);
		Assert.assertEquals(TEXT, swtLink.getText());
	}
	
	@Test
	public void testModelSetTextAfterViewerSetContentsEqualsSWTText() {
		Link swtLink = (Link) ModelSetMultipleRule(false, widget, hyperlinkRule, stringRuleText);
		Assert.assertEquals(TEXT, swtLink.getText());
	}
	
	@Test
	public void testModelSetMultipleTextBeforeViewerSetContentsEqualsSWTText() {
		Link swtLink = (Link) ModelSetMultipleRule3(true, widget, hyperlinkRule, stringRuleText, stringRuleText2);
		Assert.assertEquals(TEXT, swtLink.getText());
	}
	
	@Test
	public void testModelSetMultipleTextAfterViewerSetContentsEqualsSWTText() {
		Link swtLink = (Link) ModelSetMultipleRule3(false, widget, hyperlinkRule, stringRuleText, stringRuleText2);
		Assert.assertEquals(TEXT, swtLink.getText());
	}

	
	@Test
	public void testModelMoveTextBeforeViewerSetContentsEqualsSWTText(){
		Link swtLink = (Link) ModelMoveThreeRule(true, widget, hyperlinkRule,stringRuleText, stringRuleText2,  "text");
		Assert.assertEquals(TEXT2, swtLink.getText());
	}
	
	@Test
	public void testModelMoveTextAfterViewerSetContentsEqualsSWTText(){
		Link swtLink = (Link) ModelMoveThreeRule(false, widget, hyperlinkRule,stringRuleText, stringRuleText2,  "text");
		Assert.assertEquals(TEXT2, swtLink.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRemoveBeforeViewerSetContentsEqualsSWTText(){
		Link swtLink = (Link) ModelRemoveRuleByRemove3(true, widget, stringRuleText, stringRuleText2, hyperlinkRule, "text");
		Assert.assertEquals(TEXT2, swtLink.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRemoveAfterViewerSetContentsEqualsSWTText(){
		Link swtLink = (Link) ModelRemoveRuleByRemove3(false, widget, stringRuleText, stringRuleText2, hyperlinkRule, "text");
		Assert.assertEquals(TEXT2, swtLink.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRenameBeforeViewerSetContentsEqualsSWTText(){
		Link swtLink = (Link) ModelRemoveRuleByRename3(true, widget, stringRuleText, stringRuleText2,hyperlinkRule, "text");
		Assert.assertEquals(TEXT2, swtLink.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRenameAfterViewerSetContentsEqualsSWTText(){
		Link swtLink = (Link) ModelRemoveRuleByRename3(false, widget, stringRuleText, stringRuleText2,hyperlinkRule, "text");
		Assert.assertEquals(TEXT2, swtLink.getText());
	}
	
	
	@Test
	public void testModelModifyTextBeforeSetContentsEqualsSWTText(){
		widget.getStyleRules().add(hyperlinkRule);
		Link swtLink = (Link) ModelModifyRule(true, widget);
		Assert.assertEquals(TEXT2, swtLink.getText());
	}
	
	@Test
	public void testModelModifyTextAfterSetContentsEqualsSWTText(){
		widget.getStyleRules().add(hyperlinkRule);
		Link swtLink = (Link) ModelModifyRule(false, widget);
		Assert.assertEquals(TEXT2, swtLink.getText());
	}

}
