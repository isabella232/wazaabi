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

import org.eclipse.swt.widgets.Button;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestSelectedRadioButton extends AbstractTestWidget{
	protected static final boolean VALUE = true;
	protected static final boolean VALUE2 = false;
	private org.eclipse.wazaabi.mm.core.widgets.RadioButton widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createRadioButton();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	private org.eclipse.swt.widgets.Widget ModelSetStructuralFeatureSelected(boolean before, org.eclipse.wazaabi.mm.core.widgets.RadioButton radioButton){
		if(!before)
			viewer.setContents(widget);
		
		radioButton.setSelected(VALUE);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, radioButton);
		
	}
	
	private org.eclipse.swt.widgets.Widget ModelModifyStructuralFeatureSelected(boolean before, org.eclipse.wazaabi.mm.core.widgets.RadioButton radioButton){
		if(!before)
			viewer.setContents(widget);
		radioButton.setSelected(VALUE);
		
		if(before)
			viewer.setContents(widget);
		radioButton.setSelected(VALUE2);
		
		return SWTUtils.getWidget(viewer, radioButton);
	}
	
	@Test 
	public void testModelSetSelectedBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtRadioButton = (Button)ModelSetStructuralFeatureSelected(true, widget);
		Assert.assertEquals(VALUE,swtRadioButton.getSelection());
				
	}
	
	@Test 
	public void testModelSetSelectedAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtRadioButton = (Button)ModelSetStructuralFeatureSelected(false, widget);
		Assert.assertEquals(VALUE,swtRadioButton.getSelection());
				
	}
	
	@Test 
	public void testModelModifySelectedBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtRadioButton = (Button)ModelModifyStructuralFeatureSelected(true, widget);
		Assert.assertEquals(VALUE2,swtRadioButton.getSelection());
				
	}
	
	@Test 
	public void testModelModifySelectedAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtRadioButton = (Button)ModelModifyStructuralFeatureSelected(false, widget);
		Assert.assertEquals(VALUE2,swtRadioButton.getSelection());
				
	}

}
