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

public class TestColorRuleSwitchForegroundAndBackgroundColorLabel extends AbstractTestWidget{
	
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
		colorRuleColor.setPropertyName("foreground-color");
		colorRuleColor.setRed(RED);
		colorRuleColor.setGreen(GREEN);
		colorRuleColor.setBlue(BLUE);
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	
	protected org.eclipse.swt.widgets.Widget ModelRenameRuleFromForegroundToBackground(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(colorRuleColor);
		widget = TestUtils.renameRuleToNewRule(widget, "foreground-color", "background-color");
		if(before)
			viewer.setContents(widget);
				
		return SWTUtils.getWidget(viewer,widget); 
		
		
	}
	

	@Test
	public void testRenameForegroundColortoBackgroundColorBeforeViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRenameRuleFromForegroundToBackground(true, widget);
		Color swtColorBack = swtLabel.getBackground();
		Color swtColorFore = swtLabel.getForeground();
		Assert.assertNotNull(swtColorBack);
		Assert.assertEquals(RED, swtColorBack.getRed());
		Assert.assertEquals(GREEN, swtColorBack.getGreen());
		Assert.assertEquals(BLUE, swtColorBack.getBlue());
		
		Assert.assertNotNull(swtColorFore);
		//TODO
		//76 is the default value for color on ubuntu. This should be changed to the SWT platform default value
		Assert.assertEquals(76,swtColorFore.getBlue());
		Assert.assertEquals(76,swtColorFore.getRed());
		Assert.assertEquals(76,swtColorFore.getGreen());
	}
	
	@Test
	public void testRenameForegroundColortoBackgroundColorAfterViewerSetContentsEqualsSWTColor(){
		Label swtLabel = (Label) ModelRenameRuleFromForegroundToBackground(false, widget);
		Color swtColorBack = swtLabel.getBackground();
		Color swtColorFore = swtLabel.getForeground();
		Assert.assertNotNull(swtColorBack);
		Assert.assertEquals(RED, swtColorBack.getRed());
		Assert.assertEquals(GREEN, swtColorBack.getGreen());
		Assert.assertEquals(BLUE, swtColorBack.getBlue());
		
		//TODO
		//76 is the default value for color on ubuntu. This should be changed to the SWT platform default value
		Assert.assertEquals(76,swtColorFore.getBlue());
		Assert.assertEquals(76,swtColorFore.getRed());
		Assert.assertEquals(76,swtColorFore.getGreen());
	}
	
}
