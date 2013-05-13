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

import org.eclipse.swt.widgets.Scale;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestValueScale extends AbstractTestWidget{
	protected static final int VALUE = 75;
	protected static final int VALUE2 = 25;
	private org.eclipse.wazaabi.mm.core.widgets.Scale widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createScale();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	private org.eclipse.swt.widgets.Widget ModelSetStructuralFeatureValue(boolean before, org.eclipse.wazaabi.mm.core.widgets.Scale scale){
		if(!before)
			viewer.setContents(widget);
		scale.setValue(VALUE);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, scale);
		
	}
	
	private org.eclipse.swt.widgets.Widget ModelModifyStructuralFeatureValue(boolean before, org.eclipse.wazaabi.mm.core.widgets.Scale scale){
		if(!before)
			viewer.setContents(widget);
		scale.setValue(VALUE);
		
		if(before)
			viewer.setContents(widget);
		scale.setValue(VALUE2);
		
		return SWTUtils.getWidget(viewer, scale);
	}
	
	@Test 
	public void testModelSetValueBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Scale swtScale = (Scale)ModelSetStructuralFeatureValue(true, widget);
		Assert.assertEquals(VALUE,swtScale.getSelection());
				
	}
	
	@Test 
	public void testModelSetValueAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Scale swtScale = (Scale)ModelSetStructuralFeatureValue(false, widget);
		Assert.assertEquals(VALUE,swtScale.getSelection());
				
	}
	
	@Test 
	public void testModelModifyValueBeforeViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Scale swtScale = (Scale)ModelModifyStructuralFeatureValue(true, widget);
		Assert.assertEquals(VALUE2,swtScale.getSelection());
				
	}
	
	@Test 
	public void testModelModifyValueAfterViewerSetContentsEqualsSWTValue(){
		org.eclipse.swt.widgets.Scale swtScale = (Scale)ModelModifyStructuralFeatureValue(false, widget);
		Assert.assertEquals(VALUE2,swtScale.getSelection());
				
	}

}
