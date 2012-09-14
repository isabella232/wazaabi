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

import org.eclipse.swt.widgets.Button;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.RadioButton;
import org.junit.Assert;
import org.junit.Test;

public class TestStringRuleTextRadioButton extends AbstractTestStringRuleText{
	
	private RadioButton widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createRadioButton();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	
	@Test
	public void testModelSetTextBeforeViewerSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelSetRule(true, widget, stringRuleText);
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	@Test
	public void testModelSetTextAfterViewerSetContentsEqualsSWTText() {
		Button swtButton = (Button) ModelSetRule(false, widget, stringRuleText);
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	
	@Test
	public void testModelSetMultipleTextBeforeViewerSetContentsEqualsSWTText() {
		Button swtButton = (Button) ModelSetMultipleRule(true, widget, stringRuleText, stringRuleText2);
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	@Test
	public void testModelSetMultipleTextAfterViewerSetContentsEqualsSWTText() {
		Button swtButton = (Button) ModelSetMultipleRule(false, widget, stringRuleText, stringRuleText2);
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	@Test
	public void testModelMoveTextBeforeViewerSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelMoveRule(true, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	@Test
	public void testModelMoveTextAfterViewerSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelMoveRule(false, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRemoveBeforeViewerSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelRemoveRuleByRemove(true, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRemoveAfterViewerSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelRemoveRuleByRemove(false, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtButton.getText());
	}

	
	@Test 
	public void testModelRemoveTextByRenameBeforeViewerSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelRemoveRuleByRename(true, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	@Test 
	public void testModelRemoveTextByRenameAfterViewerSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelRemoveRuleByRename(false, widget, stringRuleText, stringRuleText2, "text");
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	
	@Test
	public void testModelModifyTextBeforeSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelModifyRule(true, widget);
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	@Test
	public void testModelModifyTextAfterSetContentsEqualsSWTText(){
		Button swtButton = (Button) ModelModifyRule(false, widget);
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	

}












