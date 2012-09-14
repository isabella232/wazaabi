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
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;


abstract public class AbstractTestStyleRule extends AbstractTestWidget {
	Action action;
	Condition condition;
	
	@Override
	public void before() {
		super.before();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelSetRule(boolean before, Widget widget, StyleRule styleRule) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule);		

		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		//Button swtButton = (Button) SWTUtils.getWidget(viewer,widget); 
		//Assert.assertEquals(expected, swtButton.getText());
	}
	
	protected org.eclipse.swt.widgets.Widget ModelSetMultipleRule(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);	
		widget.getStyleRules().add(styleRule2);

		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer,widget); 
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelSetMultipleRule3(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, StyleRule styleRule3) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);	
		widget.getStyleRules().add(styleRule2);
		widget.getStyleRules().add(styleRule3);

		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer,widget); 
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelMoveRule(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.switchFirstAndSecondRule(widget, propertyName);
		if(before)
			viewer.setContents(widget);
				
		return SWTUtils.getWidget(viewer,widget); 
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelMoveThreeRule(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, StyleRule styleRule3, String propertyName) {
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);	
		widget.getStyleRules().add(styleRule2);
		widget.getStyleRules().add(styleRule3);
		widget.getStyleRules().move(2,1);
		//widget = TestUtils.switchFirstAndSecondRule(widget, propertyName);
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer,widget); 
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRemove(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.removeFirstRuleByRemove(widget,propertyName);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRemove3(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, StyleRule styleRule3, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);
		widget.getStyleRules().add(styleRule2);
		widget.getStyleRules().add(styleRule3);
		widget = TestUtils.removeFirstRuleByRemove(widget,propertyName);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelRemoveSingleRuleByRemove(boolean before, Widget widget, StyleRule styleRule1, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);
		widget = TestUtils.removeFirstRuleByRemove(widget,propertyName);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRename(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);
		widget.getStyleRules().add(styleRule2);
		widget =  TestUtils.removeFirstRuleByRename(widget,propertyName);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
	} 
	
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRename3(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, StyleRule styleRule3, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);
		widget.getStyleRules().add(styleRule2);
		widget.getStyleRules().add(styleRule3);
		widget =  TestUtils.removeFirstRuleByRename(widget,propertyName);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
	} 
	
	protected org.eclipse.swt.widgets.Widget ModelRemoveSingleRuleByRename(boolean before, Widget widget, StyleRule styleRule1, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(styleRule1);
		widget =  TestUtils.removeFirstRuleByRename(widget,propertyName);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
	} 
	
	abstract protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget);
	
}















