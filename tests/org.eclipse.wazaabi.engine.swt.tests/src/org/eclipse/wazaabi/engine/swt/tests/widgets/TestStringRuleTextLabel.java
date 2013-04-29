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

public class TestStringRuleTextLabel extends AbstractTestStringRuleText{
	
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
	public void testModelSetTextBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelSetRule(true, widget, stringRuleText);
		Assert.assertEquals(TEXT, swtLabel.getText());
	}
	
	@Test
	public void testModelSetTextAfterViewerSetContentsEqualsSWTText() {
		Label swtLabel = (Label) ModelSetRule(false, widget, stringRuleText);
		Assert.assertEquals(TEXT, swtLabel.getText());
	}
	
	@Test
	public void testModelSetMultipleTextBeforeViewerSetContentsEqualsSWTText() {
		Label swtLabel = (Label) ModelSetMultipleRule(true, widget, stringRuleText, stringRuleText2);
		Assert.assertEquals(TEXT, swtLabel.getText());
	}
	
	@Test
	public void testModelSetMultipleTextAfterViewerSetContentsEqualsSWTText() {
		Label swtLabel = (Label) ModelSetMultipleRule(false, widget, stringRuleText, stringRuleText2);
		Assert.assertEquals(TEXT, swtLabel.getText());
	}

	
	@Test
	public void testModelMoveTextBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelMoveRule(true, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}
	
	@Test
	public void testModelMoveTextAfterViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelMoveRule(false, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRemoveBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(true, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRemoveAfterViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(false, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRenameBeforeViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(true, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRenameAfterViewerSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(false, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}
	
	
	@Test
	public void testModelModifyTextBeforeSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelModifyRule(true, widget);
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}
	
	@Test
	public void testModelModifyTextAfterSetContentsEqualsSWTText(){
		Label swtLabel = (Label) ModelModifyRule(false, widget);
		Assert.assertEquals(TEXT2, swtLabel.getText());
	}

}
