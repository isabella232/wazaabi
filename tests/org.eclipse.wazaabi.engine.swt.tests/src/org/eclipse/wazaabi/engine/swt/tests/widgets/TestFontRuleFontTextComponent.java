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
import org.eclipse.swt.widgets.Text;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Assert;
import org.junit.Test;

public class TestFontRuleFontTextComponent extends AbstractTestFontRuleFont {

	private org.eclipse.wazaabi.mm.core.widgets.TextComponent widget;

	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createTextComponent();
	}

	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.wazaabi.engine.swt.commons.tests.widgets.AbstractTestStyleRule#
	 * ModelMoveRule(boolean, org.eclipse.wazaabi.mm.core.widgets.Widget,
	 * org.eclipse.wazaabi.mm.core.styles.StyleRule,
	 * org.eclipse.wazaabi.mm.core.styles.StyleRule, java.lang.String) The
	 * following methods override those from AbtractTestStyleRules in order to
	 * be able to test that images are well disposed.
	 */
	@Override
	protected org.eclipse.swt.widgets.Widget ModelMoveRule(boolean before,
			Widget widget, StyleRule styleRule1, StyleRule styleRule2,
			String propertyName) {
		if (!before)
			viewer.setContents(widget);

		Font firstSwtResource = null;
		widget.getStyleRules().add(styleRule1);

		if (!before) {
			Text swtText = (Text) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtText.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}

		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.switchFirstAndSecondRule(widget, propertyName);

		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}

		if (before)
			viewer.setContents(widget);

		return SWTUtils.getWidget(viewer, widget);

	}

	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRemove(
			boolean before, Widget widget, StyleRule styleRule1,
			StyleRule styleRule2, String propertyName) {
		if (!before)
			viewer.setContents(widget);

		Font firstSwtResource = null;
		widget.getStyleRules().add(styleRule1);

		if (!before) {
			Text swtText = (Text) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtText.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}

		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.removeFirstRuleByRemove(widget, propertyName);

		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}

		if (before)
			viewer.setContents(widget);

		return SWTUtils.getWidget(viewer, widget);

	}

	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRename(
			boolean before, Widget widget, StyleRule styleRule1,
			StyleRule styleRule2, String propertyName) {
		if (!before)
			viewer.setContents(widget);

		Font firstSwtResource = null;
		widget.getStyleRules().add(styleRule1);
		if (!before) {
			Text swtText = (Text) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtText.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.removeFirstRuleByRename(widget, propertyName);

		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}

		if (before)
			viewer.setContents(widget);

		return SWTUtils.getWidget(viewer, widget);

	}

	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before,
			Widget widget) {
		if (!before)
			viewer.setContents(widget);

		Font firstSwtResource = null;
		widget.getStyleRules().add(fontRuleFont);

		if (!before) {
			Text swtText = (Text) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtText.getFont();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}

		FontRule str = (FontRule) widget.getFirstStyleRule("font", null);
		str.setName(FONTNAME2);
		str.setHeight(HEIGHT2);
		str.setItalic(ITALIC2);
		str.setBold(BOLD2);

		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}

		if (before)
			viewer.setContents(widget);

		return SWTUtils.getWidget(viewer, widget);

	}

	@Test
	public void testModelSetFontBeforeViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelSetRule(true, widget, fontRuleFont);
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, SWT.ITALIC);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, 0);
	}

	@Test
	public void testModelSetFontAfterViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelSetRule(false, widget, fontRuleFont);
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, SWT.ITALIC);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, 0);

	}

	@Test
	public void testModelSetMultipleFontBeforeViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelSetMultipleRule(true, widget, fontRuleFont,
				fontRuleFont2);
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, SWT.ITALIC);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, 0);
	}

	@Test
	public void testModelSetMultipleFontAfterViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelSetMultipleRule(false, widget, fontRuleFont,
				fontRuleFont2);
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, SWT.ITALIC);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, 0);
	}

	@Test
	public void testModelMoveFontBeforeViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelMoveRule(true, widget, fontRuleFont,
				fontRuleFont2, "font");
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);
	}

	@Test
	public void testModelMoveFontAfterViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelMoveRule(false, widget, fontRuleFont,
				fontRuleFont2, "font");
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);
	}

	@Test
	public void testModelRemoveFontByRemoveBeforeViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelRemoveRuleByRemove(true, widget,
				fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);

	}

	@Test
	public void testModelRemoveFontByRemoveAfterViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelRemoveRuleByRemove(false, widget,
				fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);

	}

	@Test
	public void testModelRemoveFontByRenameBeforeViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelRemoveRuleByRename(true, widget,
				fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);
	}

	@Test
	public void testModelRemoveFontByRenameAfterViewerSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelRemoveRuleByRename(false, widget,
				fontRuleFont, fontRuleFont2, "font");
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);
	}

	@Test
	public void testModelModifyFontBeforeSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelModifyRule(true, widget);
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);
	}

	@Test
	public void testModelModifyFontAfterSetContentsEqualsSWTFont() {
		Text swtText = (Text) ModelModifyRule(false, widget);
		Font swtFont = swtText.getFont();
		Assert.assertNotNull(swtFont);
		FontData[] swtFontData = swtFont.getFontData();
		Assert.assertEquals(swtFontData[0].getName(), FONTNAME2);
		Assert.assertEquals(swtFontData[0].getHeight(), HEIGHT2, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.ITALIC, 0);
		Assert.assertEquals(swtFontData[0].getStyle() & SWT.BOLD, SWT.BOLD);
	}

}
