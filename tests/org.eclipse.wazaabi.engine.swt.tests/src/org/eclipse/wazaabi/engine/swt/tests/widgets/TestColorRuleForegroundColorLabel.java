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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Label;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.engine.swt.views.SWTControlView;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Assert;
import org.junit.Test;

public class TestColorRuleForegroundColorLabel extends
		AbstractTestColorRuleForegroundColor {

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
	 * 
	 * @see org.eclipse.wazaabi.engine.swt.tests.widgets.AbstractTestStyleRule#
	 * ModelMoveRule(boolean, org.eclipse.wazaabi.mm.core.widgets.Widget,
	 * org.eclipse.wazaabi.mm.core.styles.StyleRule,
	 * org.eclipse.wazaabi.mm.core.styles.StyleRule, java.lang.String) The
	 * following methods override those from AbtractTestStyleRules in order to
	 * be able to test that images are well disposed.
	 */
	protected Color getForegroundColorFromReflection(
			SWTControlView swtControlView) throws IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		Class<?> secretClass = swtControlView.getClass();
		Class<?> parentSecretClass = secretClass.getSuperclass();
		Color badColor = null;

		try {
			Field field = parentSecretClass.getDeclaredField("foregroundColor");
			field.setAccessible(true);
			return (Color) field.get(swtControlView);

		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchFieldException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return badColor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.wazaabi.engine.swt.tests.widgets.AbstractTestStyleRule#
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

		Color swtColor = null;
		widget.getStyleRules().add(styleRule1);

		if (!before) {
			try {
				swtColor = getForegroundColorFromReflection((SWTControlView) SWTUtils
						.getWidgetView(viewer, widget));
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.switchFirstAndSecondRule(widget, propertyName);

		if (!before) {
			Assert.assertTrue(swtColor.isDisposed());
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

		Color swtColor = null;
		widget.getStyleRules().add(styleRule1);
		if (!before) {
			try {
				swtColor = getForegroundColorFromReflection((SWTControlView) SWTUtils
						.getWidgetView(viewer, widget));
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.removeFirstRuleByRemove(widget, propertyName);

		if (!before) {
			Assert.assertTrue(swtColor.isDisposed());
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

		Color swtColor = null;

		widget.getStyleRules().add(styleRule1);

		if (!before) {
			try {
				swtColor = getForegroundColorFromReflection((SWTControlView) SWTUtils
						.getWidgetView(viewer, widget));
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.removeFirstRuleByRename(widget, propertyName);

		if (!before) {
			Assert.assertTrue(swtColor.isDisposed());
		}

		if (before)
			viewer.setContents(widget);

		return SWTUtils.getWidget(viewer, widget);

	}

	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before,
			Widget widget) {
		if (!before)
			viewer.setContents(widget);

		widget.getStyleRules().add(colorRuleForegroundColor);
		/*
		if (!before) {
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtResource = swtLabel.getForeground();
			Assert.assertNotNull(firstSwtResource);
			Assert.assertFalse(firstSwtResource.isDisposed());
		}*/

		ColorRule str = (ColorRule) widget.getFirstStyleRule(
				"foreground-color", null);
		str.setRed(BGCRED2);
		str.setGreen(BGCGREEN2);
		str.setBlue(BGCBLUE2);

		/*
		if (!before) {
			Assert.assertTrue(firstSwtResource.isDisposed());
		}*/

		if (before)
			viewer.setContents(widget);

		return SWTUtils.getWidget(viewer, widget);

	}

	@Test
	public void testModelSetForegroundColorBeforeViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelSetRule(true, widget,
				colorRuleForegroundColor);
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}

	@Test
	public void testModelSetForegroundColorAfterViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelSetRule(false, widget,
				colorRuleForegroundColor);
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}

	@Test
	public void testModelSetMultipleForegroundColorBeforeViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelSetMultipleRule(true, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2);
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}

	@Test
	public void testModelSetMultipleForegroundColorAfterViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelSetMultipleRule(false, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2);
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}

	@Test
	public void testModelMoveForegroundColorBeforeViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelMoveRule(true, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2,
				"foreground-color");
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}

	@Test
	public void testModelMoveForegroundColorAfterViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelMoveRule(false, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2,
				"foreground-color");
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}

	@Test
	public void testModelRemoveForegroundColorByRemoveBeforeViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelRemoveRuleByRemove(true, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2,
				"foreground-color");
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());

	}

	@Test
	public void testModelRemoveForegroundColorByRemoveAfterViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelRemoveRuleByRemove(false, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2,
				"foreground-color");
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());

	}

	@Test
	public void testModelRemoveForegroundColorByRenameBeforeViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelRemoveRuleByRename(true, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2,
				"foreground-color");
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}

	@Test
	public void testModelRemoveForegroundColorByRenameAfterViewerSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelRemoveRuleByRename(false, widget,
				colorRuleForegroundColor, colorRuleForegroundColor2,
				"foreground-color");
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}

	@Test
	public void testModelModifyForegroundColorBeforeSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelModifyRule(true, widget);
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}

	@Test
	public void testModelModifyForegroundColorAfterSetContentsEqualsSWTColor() {
		Label swtLabel = (Label) ModelModifyRule(false, widget);
		Color swtColor = swtLabel.getForeground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}

}
