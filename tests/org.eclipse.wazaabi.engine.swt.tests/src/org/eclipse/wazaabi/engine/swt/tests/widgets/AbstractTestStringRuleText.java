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

import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Test;

public abstract class AbstractTestStringRuleText extends AbstractTestStyleRule{
	
	protected StringRule stringRuleText; 
	protected StringRule stringRuleText2;
	
	protected static final String TEXT="This is a text string";
	protected static final String TEXT2="This is a second text string";
	
	@Override
	public void before() {
		super.before();
		stringRuleText = CoreStylesFactory.eINSTANCE.createStringRule();
		stringRuleText.setPropertyName("text"); 
		stringRuleText.setValue(TEXT); //$NON-NLS-1$
		
		stringRuleText2 = CoreStylesFactory.eINSTANCE.createStringRule();
		stringRuleText2.setPropertyName("text"); 
		stringRuleText2.setValue(TEXT2); //$NON-NLS-1$
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	/*
	protected void testModelSetTextEqualsSWTText(boolean before, Widget widget) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(stringRuleText);		

		if(before)
			viewer.setContents(widget);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer,widget); 
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	protected void testModelSetMultipleTextEqualsSWTText(boolean before, Widget widget) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(stringRuleText);	
		widget.getStyleRules().add(stringRuleText2);

		if(before)
			viewer.setContents(widget);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer,widget); 
		Assert.assertEquals(TEXT, swtButton.getText());
	}
	
	protected void testModelMoveTextEqualsSWTText(boolean before, Widget widget) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(stringRuleText);	
		widget.getStyleRules().add(stringRuleText2);
		widget = TestUtils.switchFirstAndSecondRule(widget, "text");
		if(before)
			viewer.setContents(widget);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer,widget); 
		System.out.println(swtButton.getText());
		Assert.assertEquals(TEXT2, swtButton.getText());
	}
	
	protected void testModelRemoveTextByRemoveEqualsSWTText(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(stringRuleText);
		widget.getStyleRules().add(stringRuleText2);
		widget = TestUtils.removeFirstRuleByRemove(widget,"text");
		
		if(before)
			viewer.setContents(widget);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer, widget);
		Assert.assertEquals(TEXT2, swtButton.getText());
		
	}
	
	protected void testModelRemoveTextByRenameEqualsSWTText(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(stringRuleText);
		widget.getStyleRules().add(stringRuleText2);
		widget = (CheckBox) org.eclipse.pmf.wazaabi.engine.swt.tests.TestUtils.removeFirstRuleByRename(widget,"text");
		
		if(before)
			viewer.setContents(widget);
		
		Button swtButton = (Button) SWTUtils.getWidget(viewer, widget);
		Assert.assertEquals(TEXT2, swtButton.getText());
	} */
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(stringRuleText);
		StringRule str =(StringRule) widget.getStyleRules().get(0);
		str.setValue(TEXT2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		//Assert.assertEquals(TEXT2, swtButton.getText());
		
	}
	
	
	@Test
	abstract public void testModelSetTextBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelSetTextAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelSetMultipleTextBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelSetMultipleTextAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelMoveTextBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelMoveTextAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTextByRemoveBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTextByRemoveAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTextByRenameBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTextByRenameAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelModifyTextBeforeSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelModifyTextAfterSetContentsEqualsSWTText();
	
}
