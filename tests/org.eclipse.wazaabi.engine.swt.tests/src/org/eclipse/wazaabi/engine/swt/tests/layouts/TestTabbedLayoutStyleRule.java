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

import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Test;

public class TestTabbedLayoutStyleRule extends AbstractTestTabbedLayout {
	
	protected Container container; 
	
	@Override
	public void before(){
		super.before();
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
		
	}
	
	@Test
	public void testModelSetTabbedLayoutRuleBeforeSetContentsEqualsSWTCTabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetRule(true, container, tabbedLayoutRule);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
		
	}
	
	@Test
	public void testModelSetTabbedLayoutRuleAfterSetContentsEqualsSWTCTabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetRule(false, container, tabbedLayoutRule);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
		
	}
	
	@Test
	public void testModelSetMultipleTabbedLayoutRuleBeforeSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetMultipleRule(true, container, tabbedLayoutRule, tabbedLayoutRule2);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}
	
	@Test
	public void testModelSetMultipleTabbedLayoutRuleAfterSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = ModelSetMultipleRule(false, container, tabbedLayoutRule, tabbedLayoutRule2);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}
	
	@Test
	public void testModelMoveTabbedLayoutRuleBeforeViewerSetContentsEqualsSWTCtabFolder(){
		CTabFolder swtTabFolder = (CTabFolder) ModelMoveRule(true, container, tabbedLayoutRule, tabbedLayoutRule2, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}
	
	@Test
	public void testModelMoveTabbedLayoutRuleAfterViewerSetContentsEqualsSWTCtabFolder(){
		CTabFolder swtTabFolder = (CTabFolder) ModelMoveRule(false, container, tabbedLayoutRule, tabbedLayoutRule2, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}
	
	@Test 
	public void testModelRemoveTabbedLayoutRuleByRemoveBeforeViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, tabbedLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}
	
	@Test 
	public void testModelRemoveTabbedLayoutRuleByRemoveAfterViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, tabbedLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}

	
	@Test 
	public void testModelRemoveTabbedLayoutRuleByRenameBeforeViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, tabbedLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}
	
	@Test 
	public void testModelRemoveTabbedLayoutRuleByRenameAfterViewerSetContentsEqualsSWTCtabFolder(){
		org.eclipse.swt.widgets.Widget swtTabFolder = (org.eclipse.swt.widgets.Widget) ModelRemoveSingleRuleByRemove(true, container, tabbedLayoutRule, "layout");
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.widgets.Composite);
	}
	
	/* Test is pointless for the moment.
	@Test
	public void testModelModifyTabbedLayoutRuleBeforeSetContentsEqualsSWTCtabFolder(){
		CTabFolder swtTabFolder = (CTabFolder) ModelModifyRule(true, container);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}
	
	@Test
	public void testModelModifyTabbedLayoutRuleAfterSetContentsEqualsSWTCtabFolder(){
		CTabFolder swtTabFolder = (CTabFolder) ModelModifyRule(false, container);
		Assert.assertTrue(swtTabFolder instanceof org.eclipse.swt.custom.CTabFolder);
	}*/

}
