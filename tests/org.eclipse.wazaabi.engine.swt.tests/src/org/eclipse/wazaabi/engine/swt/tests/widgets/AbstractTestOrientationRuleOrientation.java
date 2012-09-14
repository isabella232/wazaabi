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
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Test;

public abstract class AbstractTestOrientationRuleOrientation extends AbstractTestStyleRule{
	protected OrientationRule orientationRuleOrientation;
	protected OrientationRule orientationRuleOrientation2;
	
	protected static final Orientation ORIENTATION = Orientation.HORIZONTAL;
	protected static final Orientation ORIENTATION2 = Orientation.VERTICAL;
	
	@Override
	public void before() {
		super.before();
		orientationRuleOrientation = CoreStylesFactory.eINSTANCE.createOrientationRule();
		orientationRuleOrientation.setPropertyName("orientation"); 
		orientationRuleOrientation.setValue(ORIENTATION); //$NON-NLS-1$
		
		orientationRuleOrientation2 = CoreStylesFactory.eINSTANCE.createOrientationRule();
		orientationRuleOrientation2.setPropertyName("orientation"); 
		orientationRuleOrientation2.setValue(ORIENTATION2); //$NON-NLS-1$
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		widget.getStyleRules().add(orientationRuleOrientation);
		OrientationRule str =(OrientationRule) widget.getStyleRules().get(0);
		str.setValue(ORIENTATION2);
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	abstract public void testModelSetOrientationBeforeViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelSetOrientationAfterViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelSetMultipleOrientationBeforeViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelSetMultipleOrientationAfterViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelMoveOrientationBeforeViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelMoveOrientationAfterViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelRemoveOrientationByRemoveBeforeViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelRemoveOrientationByRemoveAfterViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelRemoveOrientationByRenameBeforeViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelRemoveOrientationByRenameAfterViewerSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelModifyOrientationBeforeSetContentsEqualsSWTOrientation();
	
	@Test
	abstract public void testModelModifyOrientationAfterSetContentsEqualsSWTOrientation();
	
	
}
