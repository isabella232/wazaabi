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

public abstract class AbstractTestStringRuleTooltipText extends AbstractTestStyleRule{
	
	protected StringRule stringRuleTooltipText; 
	protected StringRule stringRuleTooltipText2;
	
	protected static final String TOOLTIPTEXT="This is a tooltip text string";
	protected static final String TOOLTIPTEXT2="This is a second tooltip text string";
	
	@Override
	public void before() {
		super.before();
		stringRuleTooltipText = CoreStylesFactory.eINSTANCE.createStringRule();
		stringRuleTooltipText.setPropertyName("tooltip-text"); 
		stringRuleTooltipText.setValue(TOOLTIPTEXT); //$NON-NLS-1$
		
		stringRuleTooltipText2 = CoreStylesFactory.eINSTANCE.createStringRule();
		stringRuleTooltipText2.setPropertyName("tooltip-text"); 
		stringRuleTooltipText2.setValue(TOOLTIPTEXT2); //$NON-NLS-1$
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
		
		widget.getStyleRules().add(stringRuleTooltipText);
		StringRule str =(StringRule) widget.getStyleRules().get(0);
		str.setValue(TOOLTIPTEXT2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		//Assert.assertEquals(TEXT2, swtButton.getText());
		
	}
	
	
	@Test
	abstract public void testModelSetTooltipTextBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelSetTooltipTextAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelSetMultipleTooltipTextBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelSetMultipleTooltipTextAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelMoveTooltipTextBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelMoveTooltipTextAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTooltipTextByRemoveBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTooltipTextByRemoveAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTooltipTextByRenameBeforeViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelRemoveTooltipTextByRenameAfterViewerSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelModifyTooltipTextBeforeSetContentsEqualsSWTText();
	
	@Test
	abstract public void testModelModifyTooltipTextAfterSetContentsEqualsSWTText();
	
}
