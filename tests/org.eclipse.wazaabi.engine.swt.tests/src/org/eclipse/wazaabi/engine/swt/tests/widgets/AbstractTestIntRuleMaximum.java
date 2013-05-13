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

public abstract class AbstractTestIntRuleMaximum extends AbstractTestStyleRule{
	protected IntRule intRuleMaximum;
	protected IntRule intRuleMaximum2;
	
	protected static final int MAX=120;
	protected static final int MAX2=110;
	
	@Override
	public void before() {
		super.before();
		intRuleMaximum = CoreStylesFactory.eINSTANCE.createIntRule();
		intRuleMaximum.setPropertyName("maximum"); 
		intRuleMaximum.setValue(MAX); //$NON-NLS-1$
		
		intRuleMaximum2 = CoreStylesFactory.eINSTANCE.createIntRule();
		intRuleMaximum2.setPropertyName("maximum"); 
		intRuleMaximum2.setValue(MAX2); //$NON-NLS-1$
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(intRuleMaximum);
		IntRule str =(IntRule) widget.getStyleRules().get(0);
		str.setValue(MAX2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		//Assert.assertEquals(TEXT2, swtButton.getText());
		
	}
	
	@Test
	abstract public void testModelSetMaximumBeforeViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelSetMaximumAfterViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelSetMultipleMaximumBeforeViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelSetMultipleMaximumAfterViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelMoveMaximumBeforeViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelMoveMaximumAfterViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelRemoveMaximumByRemoveBeforeViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelRemoveMaximumByRemoveAfterViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelRemoveMaximumByRenameBeforeViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelRemoveMaximumByRenameAfterViewerSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelModifyMaximumBeforeSetContentsEqualsSWTMaximum();
	
	@Test
	abstract public void testModelModifyMaximumAfterSetContentsEqualsSWTMaximum();
	
	
}
