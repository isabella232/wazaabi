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
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;


public class AbstractTestTabbedLayout extends AbstractTestStyleRule {
	
	protected TabbedLayoutRule tabbedLayoutRule;
	protected TabbedLayoutRule tabbedLayoutRule2;
	
	@Override
	public void before(){
		super.before();
		tabbedLayoutRule = CoreStylesFactory.eINSTANCE.createTabbedLayoutRule();
		tabbedLayoutRule.setPropertyName("layout");
		
		tabbedLayoutRule2 = CoreStylesFactory.eINSTANCE.createTabbedLayoutRule();
		tabbedLayoutRule2.setPropertyName("layout");
		// In the future there will be more differenciation between 2 tabbedLayoutRules than their propertyName
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(tabbedLayoutRule);
		TabbedLayoutRule tlr =(TabbedLayoutRule) widget.getStyleRules().get(0);
		tlr.setPropertyName("layout");
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}

}
