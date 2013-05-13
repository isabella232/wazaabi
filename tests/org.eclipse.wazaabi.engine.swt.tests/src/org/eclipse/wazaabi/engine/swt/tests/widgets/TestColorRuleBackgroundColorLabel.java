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
import org.eclipse.wazaabi.engine.swt.commons.views.SWTControlView;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Assert;
import org.junit.Test;

public class TestColorRuleBackgroundColorLabel extends AbstractTestColorRuleBackgroundColor{
	
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
	
	protected Color getBackgroundColorFromReflection(SWTControlView swtControlView) throws IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		Class<?> secretClass = swtControlView.getClass();
		Class<?> parentSecretClass = secretClass.getSuperclass();
		Color badColor = null;

		try {
			Field field = parentSecretClass.getDeclaredField("backgroundColor");
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
	 * @see org.eclipse.wazaabi.engine.swt.commons.tests.widgets.AbstractTestStyleRule#ModelMoveRule(boolean, org.eclipse.wazaabi.mm.core.widgets.Widget, org.eclipse.wazaabi.mm.core.styles.StyleRule, org.eclipse.wazaabi.mm.core.styles.StyleRule, java.lang.String)
	 * The following methods override those from AbtractTestStyleRules in order to be able to test that images are well disposed.
	 */
	@Override
	protected org.eclipse.swt.widgets.Widget ModelMoveRule(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName) {
		if(!before)
			viewer.setContents(widget);
		
		Color swtColor=null;
		widget.getStyleRules().add(styleRule1);
		
		if (!before) {
			try {
				swtColor = getBackgroundColorFromReflection((SWTControlView) SWTUtils
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
		
		if(!before){
			Assert.assertTrue(swtColor.isDisposed());
		}
		if(before)
			viewer.setContents(widget);
				
		return SWTUtils.getWidget(viewer,widget); 
		
	}
	
	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRemove(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		Color swtColor=null;
		widget.getStyleRules().add(styleRule1);
		if (!before) {
			try {
				swtColor = getBackgroundColorFromReflection((SWTControlView) SWTUtils
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
		widget = TestUtils.removeFirstRuleByRemove(widget,propertyName);
		
		if(!before){
			Assert.assertTrue(swtColor.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRename(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		Color swtColor=null;
		
		widget.getStyleRules().add(styleRule1);
		
		if (!before) {
			try {
				swtColor = getBackgroundColorFromReflection((SWTControlView) SWTUtils
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
		widget =  TestUtils.removeFirstRuleByRename(widget,propertyName);
		
		if(!before){
			Assert.assertTrue(swtColor.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
	
	} 
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(colorRuleBackgroundColor);
		
		ColorRule str =(ColorRule) widget.getFirstStyleRule("background-color", null);
		str.setRed(BGCRED2);
		str.setGreen(BGCGREEN2);
		str.setBlue(BGCBLUE2);
	
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	public void testModelSetBackgroundColorBeforeViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelSetRule(true, widget, colorRuleBackgroundColor);
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}
	
	@Test
	public void testModelSetBackgroundColorAfterViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelSetRule(false, widget, colorRuleBackgroundColor);
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}
	
	@Test
	public void testModelSetMultipleBackgroundColorBeforeViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelSetMultipleRule(true, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2);
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}
	
	@Test
	public void testModelSetMultipleBackgroundColorAfterViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelSetMultipleRule(false, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2);
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED, swtColor.getRed());
		Assert.assertEquals(BGCGREEN, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE, swtColor.getBlue());
	}
	
	@Test
	public void testModelMoveBackgroundColorBeforeViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelMoveRule(true, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2, "background-color");
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}
	
	@Test
	public void testModelMoveBackgroundColorAfterViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelMoveRule(false, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2, "background-color");
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}
	
	@Test
	public void testModelRemoveBackgroundColorByRemoveBeforeViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(true, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2, "background-color");
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
		
	}
	
	@Test
	public void testModelRemoveBackgroundColorByRemoveAfterViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(false, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2, "background-color");
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
		
	}
	
	@Test
	public void testModelRemoveBackgroundColorByRenameBeforeViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(true, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2, "background-color");
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}
	
	@Test
	public void testModelRemoveBackgroundColorByRenameAfterViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(false, widget, colorRuleBackgroundColor, colorRuleBackgroundColor2, "background-color");
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}
	
	@Test
	public void testModelModifyBackgroundColorBeforeSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelModifyRule(true, widget);
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}
	
	@Test
	public void testModelModifyBackgroundColorAfterSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelModifyRule(false, widget);
		Color swtColor = swtLabel.getBackground();
		Assert.assertNotNull(swtColor);
		Assert.assertEquals(BGCRED2, swtColor.getRed());
		Assert.assertEquals(BGCGREEN2, swtColor.getGreen());
		Assert.assertEquals(BGCBLUE2, swtColor.getBlue());
	}
	
	
	
	
	
	
}
