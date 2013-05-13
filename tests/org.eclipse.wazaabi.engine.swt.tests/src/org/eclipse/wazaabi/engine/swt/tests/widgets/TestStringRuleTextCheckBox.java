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
import org.eclipse.wazaabi.mm.core.widgets.CheckBox;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestStringRuleTextCheckBox extends AbstractTestStringRuleText{
	
	private CheckBox widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createCheckBox();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	/*
	private void testModelSetTextEqualsSWTText(boolean before) {
		if(!before)
			viewer.setContents(checkBox);
		
		checkBox.getStyleRules().add(stringRuleText);		

		if(before)
			viewer.setContents(checkBox);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer,checkBox); 
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	private void testModelSetMultipleTextEqualsSWTText(boolean before) {
		if(!before)
			viewer.setContents(checkBox);
		
		checkBox.getStyleRules().add(stringRuleText);	
		checkBox.getStyleRules().add(stringRuleText2);

		if(before)
			viewer.setContents(checkBox);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer,checkBox); 
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	private void testModelMoveTextEqualsSWTText(boolean before) {
		if(!before)
			viewer.setContents(checkBox);
		
		checkBox.getStyleRules().add(stringRuleText);	
		checkBox.getStyleRules().add(stringRuleText2);
		checkBox = (CheckBox) TestUtils.switchFirstAndSecondRule(checkBox, "text");
		if(before)
			viewer.setContents(checkBox);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer,checkBox); 
		System.out.println(swtButton.getText());
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	private void testModelRemoveTextByRemoveEqualsSWTText(boolean before){
		if(!before)
			viewer.setContents(checkBox);
		
		checkBox.getStyleRules().add(stringRuleText);
		checkBox.getStyleRules().add(stringRuleText2);
		checkBox = (CheckBox) org.eclipse.pmf.wazaabi.engine.swt.tests.TestUtils.removeFirstRuleByRemove(checkBox,"text");
		
		if(before)
			viewer.setContents(checkBox);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer, checkBox);
		Assert.assertEquals(TEXT2, swtButton.getText());
		
	}
	
	private void testModelRemoveTextByRenameEqualsSWTText(boolean before){
		if(!before)
			viewer.setContents(checkBox);
		
		checkBox.getStyleRules().add(stringRuleText);
		checkBox.getStyleRules().add(stringRuleText2);
		checkBox = (CheckBox) org.eclipse.pmf.wazaabi.engine.swt.tests.TestUtils.removeFirstRuleByRename(checkBox,"text");
		
		if(before)
			viewer.setContents(checkBox);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer, checkBox);
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	private void testModelModifyTextEqualsSWTText(boolean before){
		if(!before)
			viewer.setContents(checkBox);
		
		checkBox.getStyleRules().add(stringRuleText);
		StringRule str =(StringRule) checkBox.getStyleRules().get(0);
		str.setValue(TEXT2);
		
		if(before)
			viewer.setContents(checkBox);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer, checkBox);
		Assert.assertEquals(TEXT2, swtButton.getText());
		
	}
	
	@Test
	public void testModelSetTextBeforeViewerSetContentsEqualsSWTText(){
		testModelSetTextEqualsSWTText(true, checkBox);
	}
	
	@Test
	public void testModelSetTextAfterViewerSetContentsEqualsSWTText() {
		testModelSetTextEqualsSWTText(false, checkBox);
	}*/
	
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
	
	/*
	@Test
	public void testModelSetMultipleTextBeforeViewerSetContentsEqualsSWTText() {
		testModelSetMultipleTextEqualsSWTText(true, widget);
	}
	
	@Test
	public void testModelSetMultipleTextAfterViewerSetContentsEqualsSWTText() {
		testModelSetMultipleTextEqualsSWTText(false, widget);
	}
	*/
	
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
	/*
	@Test
	public void testModelMoveTextBeforeViewerSetContentsEqualsSWTText(){
		testModelMoveTextEqualsSWTText(true, widget);
	}
	
	@Test
	public void testModelMoveTextAfterViewerSetContentsEqualsSWTText(){
		testModelMoveTextEqualsSWTText(false, widget);
	}*/
	
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
	/*
	
	@Test 
	public void testModelRemoveTextByRemoveBeforeViewerSetContentsEqualsSWTText(){
		testModelRemoveTextByRemoveEqualsSWTText(true, widget);
	}
	
	@Test 
	public void testModelRemoveTextByRemoveAfterViewerSetContentsEqualsSWTText(){
		testModelRemoveTextByRemoveEqualsSWTText(false, widget);
	}
	*/
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
	/*
	@Test 
	public void testModelRemoveTextByRenameBeforeViewerSetContentsEqualsSWTText(){
		testModelRemoveTextByRenameEqualsSWTText(true, widget);
	}
	
	@Test 
	public void testModelRemoveTextByRenameAfterViewerSetContentsEqualsSWTText(){
		testModelRemoveTextByRenameEqualsSWTText(false, widget);
	}*/
	
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












