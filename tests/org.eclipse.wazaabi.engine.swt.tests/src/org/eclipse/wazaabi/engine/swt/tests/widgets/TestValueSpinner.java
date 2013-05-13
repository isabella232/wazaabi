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

import org.eclipse.swt.widgets.Spinner;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestValueSpinner extends AbstractTestWidget{
	protected static final int VALUE = 75;
	protected static final int VALUE2 = 25;
	private org.eclipse.wazaabi.mm.core.widgets.Spinner widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createSpinner();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	private org.eclipse.swt.widgets.Widget ModelSetStructuralFeatureValue(boolean before, org.eclipse.wazaabi.mm.core.widgets.Spinner progressBar){
		if(!before)
			viewer.setContents(widget);
		progressBar.setValue(VALUE);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, progressBar);
		
	}
	
	private org.eclipse.swt.widgets.Widget ModelModifyStructuralFeatureValue(boolean before, org.eclipse.wazaabi.mm.core.widgets.Spinner progressBar){
		if(!before)
			viewer.setContents(widget);
		progressBar.setValue(VALUE);
		
		if(before)
			viewer.setContents(widget);
		progressBar.setValue(VALUE2);
		
		return SWTUtils.getWidget(viewer, progressBar);
	}
	
	@Test 
	public void testModelSetValueBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Spinner swtSpinner = (Spinner)ModelSetStructuralFeatureValue(true, widget);
		Assert.assertEquals(VALUE,swtSpinner.getSelection());
				
	}
	
	@Test 
	public void testModelSetValueAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Spinner swtSpinner = (Spinner)ModelSetStructuralFeatureValue(false, widget);
		Assert.assertEquals(VALUE,swtSpinner.getSelection());
				
	}
	
	@Test 
	public void testModelModifyValueBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Spinner swtSpinner = (Spinner)ModelModifyStructuralFeatureValue(true, widget);
		Assert.assertEquals(VALUE2,swtSpinner.getSelection());
				
	}
	
	@Test 
	public void testModelModifyValueAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Spinner swtSpinner = (Spinner)ModelModifyStructuralFeatureValue(false, widget);
		Assert.assertEquals(VALUE2,swtSpinner.getSelection());
				
	}

}
