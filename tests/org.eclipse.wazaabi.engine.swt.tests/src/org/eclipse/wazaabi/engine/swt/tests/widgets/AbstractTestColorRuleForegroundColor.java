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

public abstract class AbstractTestColorRuleForegroundColor extends AbstractTestStyleRule {
	
	protected ColorRule colorRuleForegroundColor;
	protected ColorRule colorRuleForegroundColor2;
	
	protected static final int BGCRED=255;
	protected static final int BGCGREEN=0;
	protected static final int BGCBLUE=0;
	
	protected static final int BGCRED2=0;
	protected static final int BGCGREEN2=255;
	protected static final int BGCBLUE2=0;

	@Override
	public void before() {
		super.before();
		colorRuleForegroundColor = CoreStylesFactory.eINSTANCE.createColorRule();
		colorRuleForegroundColor.setPropertyName("foreground-color"); 
		colorRuleForegroundColor.setRed(BGCRED);
		colorRuleForegroundColor.setGreen(BGCGREEN);
		colorRuleForegroundColor.setBlue(BGCBLUE);
		
		colorRuleForegroundColor2 = CoreStylesFactory.eINSTANCE.createColorRule();
		colorRuleForegroundColor2.setPropertyName("foreground-color"); 
		colorRuleForegroundColor2.setRed(BGCRED2);
		colorRuleForegroundColor2.setGreen(BGCGREEN2);
		colorRuleForegroundColor2.setBlue(BGCBLUE2);
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	@Test
	abstract public void testModelSetForegroundColorBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelSetForegroundColorAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelSetMultipleForegroundColorBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelSetMultipleForegroundColorAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelMoveForegroundColorBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelMoveForegroundColorAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveForegroundColorByRemoveBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveForegroundColorByRemoveAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveForegroundColorByRenameBeforeViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelRemoveForegroundColorByRenameAfterViewerSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelModifyForegroundColorBeforeSetContentsEqualsSWTColor();
	
	@Test
	abstract public void testModelModifyForegroundColorAfterSetContentsEqualsSWTColor();
	
	
}
