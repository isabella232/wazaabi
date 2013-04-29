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

import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Test;

public class TestExpandLayoutStyleRule extends AbstractTestExpandLayout {
	
	protected Container container; 
	
	@Override
	public void before(){
		super.before();
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
		
	}
	
	@Test
	public void testModelSetExpandLayoutRuleBeforeSetContentsEqualsSWTCTabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetRule(true, container, expandLayoutRule);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
		
	}
	
	@Test
	public void testModelSetExpandLayoutRuleAfterSetContentsEqualsSWTCTabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetRule(false, container, expandLayoutRule);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
		
	}
	
	@Test
	public void testModelSetMultipleExpandLayoutRuleBeforeSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetMultipleRule(true, container, expandLayoutRule, expandLayoutRule2);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
	}
	
	@Test
	public void testModelSetMultipleExpandLayoutRuleAfterSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetMultipleRule(false, container, expandLayoutRule, expandLayoutRule2);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
	}
	
	@Test
	public void testModelMoveExpandLayoutRuleBeforeViewerSetContentsEqualsSWTCtabFolder(){
		ExpandBar swtTabFolder = (ExpandBar) ModelMoveRule(true, container, expandLayoutRule, expandLayoutRule2, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
	}
	
	@Test
	public void testModelMoveExpandLayoutRuleAfterViewerSetContentsEqualsSWTCtabFolder(){
		ExpandBar swtTabFolder = (ExpandBar) ModelMoveRule(false, container, expandLayoutRule, expandLayoutRule2, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
	}
	
	@Test 
	public void testModelRemoveExpandLayoutRuleByRemoveBeforeViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, expandLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}
	
	@Test 
	public void testModelRemoveExpandLayoutRuleByRemoveAfterViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, expandLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}

	
	@Test 
	public void testModelRemoveExpandLayoutRuleByRenameBeforeViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, expandLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}
	
	@Test 
	public void testModelRemoveExpandLayoutRuleByRenameAfterViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, expandLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}
	
	
	@Test
	public void testModelModifyExpandLayoutRuleBeforeSetContentsEqualsSWTCtabFolder(){
		ExpandBar swtTabFolder = (ExpandBar) ModelModifyRule(true, container);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
	}
	
	@Test
	public void testModelModifyExpandLayoutRuleAfterSetContentsEqualsSWTCtabFolder(){
		ExpandBar swtTabFolder = (ExpandBar) ModelModifyRule(false, container);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.ExpandBar);
	}

}
