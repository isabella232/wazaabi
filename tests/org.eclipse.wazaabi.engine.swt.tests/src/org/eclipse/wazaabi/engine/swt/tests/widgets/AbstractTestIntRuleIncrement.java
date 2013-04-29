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

public abstract class AbstractTestIntRuleIncrement extends AbstractTestStyleRule{
	protected IntRule intRuleIncrement;
	protected IntRule intRuleIncrement2;
	
	protected static final int INC=5;
	protected static final int INC2=6;
	
	@Override
	public void before() {
		super.before();
		intRuleIncrement = CoreStylesFactory.eINSTANCE.createIntRule();
		intRuleIncrement.setPropertyName("increment"); 
		intRuleIncrement.setValue(INC); //$NON-NLS-1$
		
		intRuleIncrement2 = CoreStylesFactory.eINSTANCE.createIntRule();
		intRuleIncrement2.setPropertyName("increment"); 
		intRuleIncrement2.setValue(INC2); //$NON-NLS-1$
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(intRuleIncrement);
		IntRule str =(IntRule) widget.getStyleRules().get(0);
		str.setValue(INC2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	abstract public void testModelSetIncrementBeforeViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelSetIncrementAfterViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelSetMultipleIncrementBeforeViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelSetMultipleIncrementAfterViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelMoveIncrementBeforeViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelMoveIncrementAfterViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelRemoveIncrementByRemoveBeforeViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelRemoveIncrementByRemoveAfterViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelRemoveIncrementByRenameBeforeViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelRemoveIncrementByRenameAfterViewerSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelModifyIncrementBeforeSetContentsEqualsSWTIncrement();
	
	@Test
	abstract public void testModelModifyIncrementAfterSetContentsEqualsSWTIncrement();
	
	
}
