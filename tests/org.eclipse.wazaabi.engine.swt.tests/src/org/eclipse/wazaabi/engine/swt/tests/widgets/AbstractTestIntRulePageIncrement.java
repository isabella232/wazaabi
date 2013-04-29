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
import org.eclipse.wazaabi.mm.core.styles.IntRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Test;

public abstract class AbstractTestIntRulePageIncrement extends AbstractTestStyleRule{
	protected IntRule intRulePageIncrement;
	protected IntRule intRulePageIncrement2;
	
	protected static final int PINC=1;
	protected static final int PINC2=2;
	
	@Override
	public void before() {
		super.before();
		intRulePageIncrement = CoreStylesFactory.eINSTANCE.createIntRule();
		intRulePageIncrement.setPropertyName("pageIncrement"); 
		intRulePageIncrement.setValue(PINC); //$NON-NLS-1$
		
		intRulePageIncrement2 = CoreStylesFactory.eINSTANCE.createIntRule();
		intRulePageIncrement2.setPropertyName("pageIncrement"); 
		intRulePageIncrement2.setValue(PINC2); //$NON-NLS-1$
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(intRulePageIncrement);
		IntRule str =(IntRule) widget.getStyleRules().get(0);
		str.setValue(PINC2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	abstract public void testModelSetPageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelSetPageIncrementAfterViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelSetMultiplePageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelSetMultiplePageIncrementAfterViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelMovePageIncrementBeforeViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelMovePageIncrementAfterViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelRemovePageIncrementByRemoveBeforeViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelRemovePageIncrementByRemoveAfterViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelRemovePageIncrementByRenameBeforeViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelRemovePageIncrementByRenameAfterViewerSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelModifyPageIncrementBeforeSetContentsEqualsSWTPageIncrement();
	
	@Test
	abstract public void testModelModifyPageIncrementAfterSetContentsEqualsSWTPageIncrement();
	
	
}
