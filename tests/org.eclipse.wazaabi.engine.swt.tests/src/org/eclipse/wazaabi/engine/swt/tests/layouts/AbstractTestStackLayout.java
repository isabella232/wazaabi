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

package org.eclipse.wazaabi.engine.swt.tests.layouts;

import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.widgets.AbstractTestStyleRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;


public class AbstractTestStackLayout extends AbstractTestStyleRule {
	
	protected StackLayoutRule stackLayoutRule;
	protected StackLayoutRule stackLayoutRule2;
	
	@Override
	public void before(){
		super.before();
		stackLayoutRule = CoreStylesFactory.eINSTANCE.createStackLayoutRule();
		stackLayoutRule.setPropertyName("layout");
		stackLayoutRule.setTop(0);
		
		stackLayoutRule2 = CoreStylesFactory.eINSTANCE.createStackLayoutRule();
		stackLayoutRule2.setPropertyName("layout");
		stackLayoutRule2.setTop(1);
		// In the future there will be more differenciation between 2 tabbedLayoutRules than their propertyName
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(stackLayoutRule);
		StackLayoutRule tlr =(StackLayoutRule) widget.getFirstStyleRule("layout", null);
		tlr.setPropertyName("layout");
		tlr.setTop(1);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}

}
