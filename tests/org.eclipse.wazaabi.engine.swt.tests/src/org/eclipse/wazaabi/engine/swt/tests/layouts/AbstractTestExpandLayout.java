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
import org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;


public class AbstractTestExpandLayout extends AbstractTestStyleRule {
	
	protected ExpandLayoutRule expandLayoutRule;
	protected ExpandLayoutRule expandLayoutRule2;
	
	@Override
	public void before(){
		super.before();
		expandLayoutRule = CoreStylesFactory.eINSTANCE.createExpandLayoutRule();
		expandLayoutRule.setPropertyName("layout");
		
		expandLayoutRule2 = CoreStylesFactory.eINSTANCE.createExpandLayoutRule();
		expandLayoutRule2.setPropertyName("layout");
		// In the future there will be more differenciation between 2 tabbedLayoutRules than their propertyName
		
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(expandLayoutRule);
		ExpandLayoutRule tlr =(ExpandLayoutRule) widget.getStyleRules().get(0);
		tlr.setPropertyName("layout");
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}

}
