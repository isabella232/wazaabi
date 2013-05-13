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

import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.junit.Test;

public abstract class AbstractTestColorRuleBackgroundColor extends AbstractTestStyleRule {
	
	protected ColorRule colorRuleBackgroundColor;
	protected ColorRule colorRuleBackgroundColor2;
	
	protected static final int BGCRED=255;
	protected static final int BGCGREEN=0;
	protected static final int BGCBLUE=0;
	
	protected static final int BGCRED2=0;
	protected static final int BGCGREEN2=255;
	protected static final int BGCBLUE2=0;

	@Override
	public void before() {
		super.before();
		colorRuleBackgroundColor = CoreStylesFactory.eINSTANCE.createColorRule();
		colorRuleBackgroundColor.setPropertyName("background-color"); 
		colorRuleBackgroundColor.setRed(BGCRED);
		colorRuleBackgroundColor.setGreen(BGCGREEN);
		colorRuleBackgroundColor.setBlue(BGCBLUE);
		
		colorRuleBackgroundColor2 = CoreStylesFactory.eINSTANCE.createColorRule();
		colorRuleBackgroundColor2.setPropertyName("background-color"); 
		colorRuleBackgroundColor2.setRed(BGCRED2);
		colorRuleBackgroundColor2.setGreen(BGCGREEN2);
		colorRuleBackgroundColor2.setBlue(BGCBLUE2);
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	@Test
	abstract public void testModelSetBackgroundColorBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelSetBackgroundColorAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelSetMultipleBackgroundColorBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelSetMultipleBackgroundColorAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelMoveBackgroundColorBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelMoveBackgroundColorAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveBackgroundColorByRemoveBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveBackgroundColorByRemoveAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveBackgroundColorByRenameBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveBackgroundColorByRenameAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelModifyBackgroundColorBeforeSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelModifyBackgroundColorAfterSetContentsEqualsSWTColor();
	
	
}
