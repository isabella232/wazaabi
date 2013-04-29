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

public class TestSelectedCheckBox extends AbstractTestWidget{
	protected static final boolean VALUE = true;
	protected static final boolean VALUE2 = false;
	private org.eclipse.wazaabi.mm.core.widgets.CheckBox widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createCheckBox();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	private org.eclipse.swt.widgets.Widget ModelSetStructuralFeatureSelected(boolean before, org.eclipse.wazaabi.mm.core.widgets.CheckBox checkBox){
		if(!before)
			viewer.setContents(widget);
		
		checkBox.setSelected(VALUE);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, checkBox);
		
	}
	
	private org.eclipse.swt.widgets.Widget ModelModifyStructuralFeatureSelected(boolean before, org.eclipse.wazaabi.mm.core.widgets.CheckBox checkBox){
		if(!before)
			viewer.setContents(widget);
		checkBox.setSelected(VALUE);
		
		if(before)
			viewer.setContents(widget);
		checkBox.setSelected(VALUE2);
		
		return SWTUtils.getWidget(viewer, checkBox);
	}
	
	@Test 
	public void testModelSetSelectedBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtCheckBox = (Button)ModelSetStructuralFeatureSelected(true, widget);
		Assert.assertEquals(VALUE,swtCheckBox.getSelection());
				
	}
	
	@Test 
	public void testModelSetSelectedAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtCheckBox = (Button)ModelSetStructuralFeatureSelected(false, widget);
		Assert.assertEquals(VALUE,swtCheckBox.getSelection());
				
	}
	
	@Test 
	public void testModelModifySelectedBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtCheckBox = (Button)ModelModifyStructuralFeatureSelected(true, widget);
		Assert.assertEquals(VALUE2,swtCheckBox.getSelection());
				
	}
	
	@Test 
	public void testModelModifySelectedAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Button swtCheckBox = (Button)ModelModifyStructuralFeatureSelected(false, widget);
		Assert.assertEquals(VALUE2,swtCheckBox.getSelection());
				
	}

}
