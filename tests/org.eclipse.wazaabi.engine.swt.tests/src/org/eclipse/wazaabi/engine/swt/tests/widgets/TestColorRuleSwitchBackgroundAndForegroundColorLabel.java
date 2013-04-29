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

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Label;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Assert;
import org.junit.Test;

public class TestColorRuleSwitchBackgroundAndForegroundColorLabel extends AbstractTestWidget{
	
	private org.eclipse.wazaabi.mm.core.widgets.Label widget;
	
	protected ColorRule colorRuleColor;
	
	protected static final int RED=255;
	protected static final int GREEN=0;
	protected static final int BLUE=0;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createLabel();
		colorRuleColor = CoreStylesFactory.eINSTANCE.createColorRule();
		colorRuleColor.setPropertyName("background-color");
		colorRuleColor.setRed(RED);
		colorRuleColor.setGreen(GREEN);
		colorRuleColor.setBlue(BLUE);
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	
	protected org.eclipse.swt.widgets.Widget ModelRenameRuleFromBackgroundToForeground(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(colorRuleColor);
		widget = TestUtils.renameRuleToNewRule(widget, "background-color", "foreground-color");
		if(before)
			viewer.setContents(widget);
				
		return SWTUtils.getWidget(viewer,widget); 
		
		
	}
	

	@Test
	public void testRenameBackgroundColortoForegroundColorBeforeViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRenameRuleFromBackgroundToForeground(true, widget);
		//Color swtColorBack = swtLabel.getBackground();
		Color swtColorFore = swtLabel.getForeground();
		Color swtColorBack = swtLabel.getBackground();

		Assert.assertNotNull(swtColorFore);
		Assert.assertEquals(RED, swtColorFore.getRed());
		Assert.assertEquals(GREEN, swtColorFore.getGreen());
		Assert.assertEquals(BLUE, swtColorFore.getBlue());
		
		Assert.assertNotNull(swtColorBack);
		// TODO
		// 76 is the default value for color on ubuntu. This should be changed
		// to the SWT platform default value
		Assert.assertEquals(240, swtColorBack.getBlue());
		Assert.assertEquals(242, swtColorBack.getRed());
		Assert.assertEquals(241, swtColorBack.getGreen());
	}
	
	@Test
	public void testRenameBackgroundColortoForegroundColorAfterViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRenameRuleFromBackgroundToForeground(false, widget);
		//Color swtColorBack = swtLabel.getBackground();
		Color swtColorFore = swtLabel.getForeground();
		Color swtColorBack = swtLabel.getBackground();

		Assert.assertNotNull(swtColorFore);
		Assert.assertEquals(RED, swtColorFore.getRed());
		Assert.assertEquals(GREEN, swtColorFore.getGreen());
		Assert.assertEquals(BLUE, swtColorFore.getBlue());
		
		Assert.assertNotNull(swtColorBack);
		// TODO
		// 76 is the default value for color on ubuntu. This should be changed
		// to the SWT platform default value
		Assert.assertEquals(240, swtColorBack.getBlue());
		Assert.assertEquals(242, swtColorBack.getRed());
		Assert.assertEquals(241, swtColorBack.getGreen());
	}
	
}
