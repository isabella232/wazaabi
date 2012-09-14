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

package org.eclipse.wazaabi.engine.swt.tests.layouts;

import junit.framework.Assert;

import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Test;

public class TestStackLayoutStyleRule extends AbstractTestStackLayout {
	
	protected Container container; 
	
	@Override
	public void before(){
		super.before();
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
		
	}
	
	@Test
	public void testModelSetStackLayoutRuleBeforeSetContentsEqualsSWTCTabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = ModelSetRule(true, container, stackLayoutRule);
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)swtContainer).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
		
	}
	
	@Test
	public void testModelSetStackLayoutRuleAfterSetContentsEqualsSWTCTabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = ModelSetRule(false, container, stackLayoutRule);
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)swtContainer).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
	}
	
	@Test
	public void testModelSetMultipleStackLayoutRuleBeforeSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = ModelSetMultipleRule(true, container, stackLayoutRule, stackLayoutRule2);
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)swtContainer).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
	}
	
	@Test
	public void testModelSetMultipleStackLayoutRuleAfterSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = ModelSetMultipleRule(false, container, stackLayoutRule, stackLayoutRule2);
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)swtContainer).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
	}
	
	@Test
	public void testModelMoveStackLayoutRuleBeforeViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = ModelMoveRule(true, container, stackLayoutRule, stackLayoutRule2, "layout");
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)swtContainer).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
	}
	
	@Test
	public void testModelMoveStackLayoutRuleAfterViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = ModelMoveRule(false, container, stackLayoutRule, stackLayoutRule2, "layout");
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)swtContainer).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
	}
	
	@Test 
	public void testModelRemoveStackLayoutRuleByRemoveBeforeViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, stackLayoutRule, "layout");
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
	}
	
	@Test 
	public void testModelRemoveStackLayoutRuleByRemoveAfterViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, stackLayoutRule, "layout");
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
	}

	
	@Test 
	public void testModelRemoveStackLayoutRuleByRenameBeforeViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, stackLayoutRule, "layout");
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
	}
	
	@Test 
	public void testModelRemoveStackLayoutRuleByRenameAfterViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtContainer = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, stackLayoutRule, "layout");
		Assert.assertTrue(swtContainer instanceof org.eclipse.swt.widgets.Composite);
	}
	
	/* Test is pointless for the moment.
	@Test
	public void testModelModifyStackLayoutRuleBeforeSetContentsEqualsSWTCtabFolder(){
		CTabFolder swtTabFolder = (CTabFolder) ModelModifyRule(true, container);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}
	
	@Test
	public void testModelModifyStackLayoutRuleAfterSetContentsEqualsSWTCtabFolder(){
		CTabFolder swtTabFolder = (CTabFolder) ModelModifyRule(false, container);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}*/

}
