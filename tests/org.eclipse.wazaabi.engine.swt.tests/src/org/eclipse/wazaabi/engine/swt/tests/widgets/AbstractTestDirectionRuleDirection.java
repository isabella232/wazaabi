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

import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.Direction;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.DirectionRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Test;

public abstract class AbstractTestDirectionRuleDirection extends AbstractTestStyleRule {
	
	protected DirectionRule directionRuleDirection;
	protected DirectionRule directionRuleDirection2;
	
	@Override
	public void before() {
		super.before();
		directionRuleDirection = CoreStylesFactory.eINSTANCE.createDirectionRule();
		directionRuleDirection.setPropertyName("direction"); 
		directionRuleDirection.setValue(Direction.LEFT_TO_RIGHT);
		
		directionRuleDirection2 = CoreStylesFactory.eINSTANCE.createDirectionRule();
		directionRuleDirection2.setPropertyName("direction"); 
		directionRuleDirection2.setValue(Direction.RIGHT_TO_LEFT);
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(directionRuleDirection);
		DirectionRule str =(DirectionRule) widget.getStyleRules().get(0);
		str.setValue(directionRuleDirection2.getValue());
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	abstract public void testModelSetDirectionBeforeViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelSetDirectionAfterViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelSetMultipleDirectionBeforeViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelSetMultipleDirectionAfterViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelMoveDirectionBeforeViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelMoveDirectionAfterViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelRemoveDirectionByRemoveBeforeViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelRemoveDirectionByRemoveAfterViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelRemoveDirectionByRenameBeforeViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelRemoveDirectionByRenameAfterViewerSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelModifyDirectionBeforeSetContentsEqualsSWTStyle();
	
	@Test
	abstract public void testModelModifyDirectionAfterSetContentsEqualsSWTStyle();
	
	
}
