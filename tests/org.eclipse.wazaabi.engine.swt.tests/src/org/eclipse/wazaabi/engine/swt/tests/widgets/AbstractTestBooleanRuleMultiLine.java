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
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Test;

public abstract class AbstractTestBooleanRuleMultiLine extends AbstractTestStyleRule{
	protected BooleanRule booleanRuleMultiLine;
	protected BooleanRule booleanRuleMultiLine2;
	
	protected static final boolean ML=true;
	protected static final boolean ML2=false;
	
	@Override
	public void before() {
		super.before();
		booleanRuleMultiLine = CoreStylesFactory.eINSTANCE.createBooleanRule();
		booleanRuleMultiLine.setPropertyName("multi-line"); 
		booleanRuleMultiLine.setValue(ML); //$NON-NLS-1$
		
		booleanRuleMultiLine2 = CoreStylesFactory.eINSTANCE.createBooleanRule();
		booleanRuleMultiLine2.setPropertyName("multi-line"); 
		booleanRuleMultiLine2.setValue(ML2); //$NON-NLS-1$
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(booleanRuleMultiLine);
		BooleanRule str =(BooleanRule) widget.getStyleRules().get(0);
		str.setValue(ML2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	abstract public void testModelSetMultiLineBeforeViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelSetMultiLineAfterViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelSetMultipleMultiLineBeforeViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelSetMultipleMultiLineAfterViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelMoveMultiLineBeforeViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelMoveMultiLineAfterViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelRemoveMultiLineByRemoveBeforeViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelRemoveMultiLineByRemoveAfterViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelRemoveMultiLineByRenameBeforeViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelRemoveMultiLineByRenameAfterViewerSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelModifyMultiLineBeforeSetContentsEqualsSWTMultiLine();
	
	@Test
	abstract public void testModelModifyMultiLineAfterSetContentsEqualsSWTMultiLine();
	
	
}
