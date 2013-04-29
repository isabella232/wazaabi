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

import org.eclipse.swt.widgets.Text;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestTextTextComponent extends AbstractTestWidget{
	protected static final String VALUE = "text1";
	protected static final String VALUE2 = "text2";
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
	
	private org.eclipse.swt.widgets.Widget ModelSetStructuralFeatureValue(boolean before, org.eclipse.wazaabi.mm.core.widgets.TextComponent textComponent){
		if(!before)
			viewer.setContents(widget);
		widget.setText(VALUE);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	private org.eclipse.swt.widgets.Widget ModelModifyStructuralFeatureValue(boolean before, org.eclipse.wazaabi.mm.core.widgets.TextComponent textComponent){
		if(!before)
			viewer.setContents(widget);
		textComponent.setText(VALUE);
		
		if(before)
			viewer.setContents(widget);
		textComponent.setText(VALUE2);
		
		return SWTUtils.getWidget(viewer, textComponent);
	}
	
	@Test 
	public void testModelSetValueBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Text swtText = (Text)ModelSetStructuralFeatureValue(true, widget);
		Assert.assertEquals(VALUE,swtText.getText());
				
	}
	
	@Test 
	public void testModelSetValueAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Text swtText = (Text)ModelSetStructuralFeatureValue(false, widget);
		Assert.assertEquals(VALUE,swtText.getText());
				
	}
	
	@Test 
	public void testModelModifyValueBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Text swtText = (Text)ModelModifyStructuralFeatureValue(true, widget);
		Assert.assertEquals(VALUE2,swtText.getText());
				
	}
	
	@Test 
	public void testModelModifyValueAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Text swtText = (Text)ModelModifyStructuralFeatureValue(false, widget);
		Assert.assertEquals(VALUE2,swtText.getText());
				
	}

}
