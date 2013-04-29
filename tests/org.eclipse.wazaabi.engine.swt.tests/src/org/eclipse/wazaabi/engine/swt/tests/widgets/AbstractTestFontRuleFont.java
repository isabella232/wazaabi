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

import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.junit.Test;

public abstract class AbstractTestFontRuleFont extends AbstractTestStyleRule {
	
	protected FontRule fontRuleFont;
	protected FontRule fontRuleFont2;
	
	protected static final String FONTNAME="Arial";
	protected static final int HEIGHT=14;
	protected static final boolean ITALIC=true;
	protected static final boolean BOLD=false;
	
	protected static final String FONTNAME2="Helvetica";
	protected static final int HEIGHT2=15;
	protected static final boolean ITALIC2=false;
	protected static final boolean BOLD2=true;

	@Override
	public void before() {
		super.before();
		fontRuleFont = CoreStylesFactory.eINSTANCE.createFontRule();
		fontRuleFont.setPropertyName("font"); 
		fontRuleFont.setName(FONTNAME);
		fontRuleFont.setHeight(HEIGHT);
		fontRuleFont.setItalic(ITALIC);
		fontRuleFont.setBold(BOLD);
	
		
		fontRuleFont2 = CoreStylesFactory.eINSTANCE.createFontRule();
		fontRuleFont2.setPropertyName("font"); 
		fontRuleFont2.setName(FONTNAME2);
		fontRuleFont2.setHeight(HEIGHT2);
		fontRuleFont2.setItalic(ITALIC2);
		fontRuleFont2.setBold(BOLD2);
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	@Test
	abstract public void testModelSetFontBeforeViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelSetFontAfterViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelSetMultipleFontBeforeViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelSetMultipleFontAfterViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelMoveFontBeforeViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelMoveFontAfterViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelRemoveFontByRemoveBeforeViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelRemoveFontByRemoveAfterViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelRemoveFontByRenameBeforeViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelRemoveFontByRenameAfterViewerSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelModifyFontBeforeSetContentsEqualsSWTFont();
	
	@Test
	abstract public void testModelModifyFontAfterSetContentsEqualsSWTFont();
	
	
}
