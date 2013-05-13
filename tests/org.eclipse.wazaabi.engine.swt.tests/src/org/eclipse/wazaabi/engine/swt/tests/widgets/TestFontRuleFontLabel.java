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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Label;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Assert;
import org.junit.Test;

public class TestFontRuleFontLabel extends AbstractTestFontRuleFont{
	
	private org.eclipse.wazaabi.mm.core.widgets.Label widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createLabel();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wazaabi.engine.swt.commons.tests.widgets.AbstractTestStyleRule#ModelMoveRule(boolean, org.eclipse.wazaabi.mm.core.widgets.Widget, org.eclipse.wazaabi.mm.core.styles.StyleRule, org.eclipse.wazaabi.mm.core.styles.StyleRule, java.lang.String)
	 * The following methods override those from AbtractTestStyleRules in order to be able to test that images are well disposed.
	 */
	@Override
	protected org.eclipse.swt.widgets.Widget ModelMoveRule(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName) {
		if(!before)
			viewer.setContents(widget);
		
		Font firstSwtResource=null;
		widget.getStyleRules().add(styleRule1);

		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtLabel.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}
		
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.switchFirstAndSecondRule(widget, propertyName);
		
		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
				
		return SWTUtils.getWidget(viewer,widget); 
		
	}
	
	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRemove(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		Font firstSwtResource=null;
		widget.getStyleRules().add(styleRule1);
		
		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtLabel.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}
		
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.removeFirstRuleByRemove(widget,propertyName);
		
		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRename(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		Font firstSwtResource=null;
		widget.getStyleRules().add(styleRule1);
		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtLabel.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}
		widget.getStyleRules().add(styleRule2);
		widget =  TestUtils.removeFirstRuleByRename(widget,propertyName);
		
		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
	
	} 
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		Font firstSwtResource=null;
		widget.getStyleRules().add(fontRuleFont);
		
		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtLabel.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}
		
		FontRule str =(FontRule) widget.getFirstStyleRule("font", null);
		str.setName(FONTNAME2);
		str.setHeight(HEIGHT2);
		str.setItalic(ITALIC2);
		str.setBold(BOLD2);
		
		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	public void testModelSetFontBeforeViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelSetRule(true, widget, fontRuleFont);
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, SWT.ITALIC );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, 0);
		System.out.println(swtLabel.getForeground());
	}
	
	@Test
	public void testModelSetFontAfterViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelSetRule(false, widget, fontRuleFont);
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, SWT.ITALIC );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, 0);
		
	}
	
	@Test
	public void testModelSetMultipleFontBeforeViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelSetMultipleRule(true, widget, fontRuleFont, fontRuleFont2);
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, SWT.ITALIC );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, 0);
	}
	
	@Test
	public void testModelSetMultipleFontAfterViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelSetMultipleRule(false, widget, fontRuleFont, fontRuleFont2);
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, SWT.ITALIC );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, 0);
	}
	
	@Test
	public void testModelMoveFontBeforeViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelMoveRule(true, widget, fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
	}
	
	@Test
	public void testModelMoveFontAfterViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelMoveRule(false, widget, fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
	}
	
	@Test
	public void testModelRemoveFontByRemoveBeforeViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(true, widget, fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
		
	}
	
	@Test
	public void testModelRemoveFontByRemoveAfterViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(false, widget, fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
		
	}
	
	@Test
	public void testModelRemoveFontByRenameBeforeViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(true, widget, fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
	}
	
	@Test
	public void testModelRemoveFontByRenameAfterViewerSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(false, widget, fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
	}
	
	@Test
	public void testModelModifyFontBeforeSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelModifyRule(true, widget);
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
	}
	
	@Test
	public void testModelModifyFontAfterSetContentsEqualsSWTFont(){
		Label swtLabel = (Label) ModelModifyRule(false, widget);
		Font swtFont = swtLabel.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.ITALIC, 0 );
		Assert.assertEquals(swtFontData[0].getStyle()&SWT.BOLD, SWT.BOLD);
	}
	
	
	
	
	
	
}
