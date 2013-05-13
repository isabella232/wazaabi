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

public abstract class AbstractTestIntRuleMinimum extends AbstractTestStyleRule{
	protected IntRule intRuleMinimum;
	protected IntRule intRuleMinimum2;
	
	protected static final int MIN=1;
	protected static final int MIN2=2;
	
	@Override
	public void before() {
		super.before();
		intRuleMinimum = CoreStylesFactory.eINSTANCE.createIntRule();
		intRuleMinimum.setPropertyName("minimum"); 
		intRuleMinimum.setValue(MIN); //$NON-NLS-1$
		
		intRuleMinimum2 = CoreStylesFactory.eINSTANCE.createIntRule();
		intRuleMinimum2.setPropertyName("minimum"); 
		intRuleMinimum2.setValue(MIN2); //$NON-NLS-1$
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(intRuleMinimum);
		IntRule str =(IntRule) widget.getStyleRules().get(0);
		str.setValue(MIN2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		//Assert.assertEquals(TEXT2, swtButton.getText());
		
	}
	
	@Test
	abstract public void testModelSetMinimumBeforeViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelSetMinimumAfterViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelSetMultipleMinimumBeforeViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelSetMultipleMinimumAfterViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelMoveMinimumBeforeViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelMoveMinimumAfterViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelRemoveMinimumByRemoveBeforeViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelRemoveMinimumByRemoveAfterViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelRemoveMinimumByRenameBeforeViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelRemoveMinimumByRenameAfterViewerSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelModifyMinimumBeforeSetContentsEqualsSWTMinimum();
	
	@Test
	abstract public void testModelModifyMinimumAfterSetContentsEqualsSWTMinimum();
	
	
}
