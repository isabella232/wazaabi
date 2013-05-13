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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestDirectionRuleDirectionLabel extends AbstractTestDirectionRuleDirection{
	
	private org.eclipse.wazaabi.mm.core.widgets.Label widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createLabel();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	
	@Test
	public void testModelSetDirectionBeforeViewerSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelSetRule(true, widget, directionRuleDirection);
		Assert.assertEquals(SWT.LEFT_TO_RIGHT, swtLabel.getStyle()&SWT.LEFT_TO_RIGHT);
	}
	
	@Test
	public void testModelSetDirectionAfterViewerSetContentsEqualsSWTStyle() {
		Label swtLabel = (Label) ModelSetRule(false, widget, directionRuleDirection);
		Assert.assertEquals(SWT.LEFT_TO_RIGHT, swtLabel.getStyle()&SWT.LEFT_TO_RIGHT);
	}
	
	
	@Test
	public void testModelSetMultipleDirectionBeforeViewerSetContentsEqualsSWTStyle() {
		Label swtLabel = (Label) ModelSetMultipleRule(true, widget, directionRuleDirection, directionRuleDirection2);
		Assert.assertEquals(SWT.LEFT_TO_RIGHT, swtLabel.getStyle()&SWT.LEFT_TO_RIGHT);
	}
	
	@Test
	public void testModelSetMultipleDirectionAfterViewerSetContentsEqualsSWTStyle() {
		Label swtLabel = (Label) ModelSetMultipleRule(false, widget, directionRuleDirection, directionRuleDirection2);
		Assert.assertEquals(SWT.LEFT_TO_RIGHT, swtLabel.getStyle()&SWT.LEFT_TO_RIGHT);
	}
	
	@Test
	public void testModelMoveDirectionBeforeViewerSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelMoveRule(true, widget, directionRuleDirection, directionRuleDirection2, "direction");
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}
	
	@Test
	public void testModelMoveDirectionAfterViewerSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelMoveRule(false, widget, directionRuleDirection, directionRuleDirection2, "direction");
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}
	
	@Test 
	public void testModelRemoveDirectionByRemoveBeforeViewerSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(true, widget, directionRuleDirection, directionRuleDirection2, "direction");
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}
	
	@Test 
	public void testModelRemoveDirectionByRemoveAfterViewerSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(false, widget, directionRuleDirection, directionRuleDirection2, "direction");
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}

	
	@Test 
	public void testModelRemoveDirectionByRenameBeforeViewerSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(true, widget, directionRuleDirection, directionRuleDirection2, "direction");
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}
	
	@Test 
	public void testModelRemoveDirectionByRenameAfterViewerSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(false, widget, directionRuleDirection, directionRuleDirection2, "direction");
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}
	
	
	@Test
	public void testModelModifyDirectionBeforeSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelModifyRule(true, widget);
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}
	
	@Test
	public void testModelModifyDirectionAfterSetContentsEqualsSWTStyle(){
		Label swtLabel = (Label) ModelModifyRule(false, widget);
		Assert.assertEquals(SWT.RIGHT_TO_LEFT, swtLabel.getStyle()&SWT.RIGHT_TO_LEFT);
	}

}
