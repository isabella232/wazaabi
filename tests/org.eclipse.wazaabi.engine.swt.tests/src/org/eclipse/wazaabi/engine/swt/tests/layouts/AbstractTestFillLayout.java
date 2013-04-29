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

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public abstract class AbstractTestFillLayout extends AbstractTestLayout {
	
	private static final Orientation LAYOUT1_ORIENTATION = Orientation.HORIZONTAL;
	private static final Orientation LAYOUT2_ORIENTATION = Orientation.VERTICAL;
	
	protected FillLayoutRule fillLayoutRule1; 
	protected FillLayoutRule fillLayoutRule2;
	
	@Override
	public void before() {
		super.before();
		fillLayoutRule1 = SWTStylesFactory.eINSTANCE.createFillLayoutRule();
		fillLayoutRule1.setPropertyName("layout"); //$NON-NLS-1$
		fillLayoutRule1.setType(LAYOUT1_ORIENTATION);
		
		fillLayoutRule2 = SWTStylesFactory.eINSTANCE.createFillLayoutRule();
		fillLayoutRule2.setPropertyName("layout"); //$NON-NLS-1$
		fillLayoutRule2.setType(LAYOUT2_ORIENTATION);
		
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
	}

	public void createWazaabiFillLayout(boolean before, String layoutData) {
		createWazaabiLayout(before, layoutData, fillLayoutRule1);
	}
	
	public void createWazaabiFillLayoutAndRemoveButtonTest(boolean before, String layoutData) {
		createWazaabiLayoutAndRemoveButtonTest(before, layoutData, fillLayoutRule1);
	}
	
	public void createSWTFillLayoutOneButton() {
		createSWTWidgetOneButton();
		
		FillLayout fillLayout = new FillLayout();
		fillLayout.type = LAYOUT1_ORIENTATION.getValue();
		swtComposite.setLayout(fillLayout);
		
		swtShell.open();
	}
	
	public void createSWTFillLayoutTwoButtons() {
		createSWTWidgetTwoButtons();
				
		FillLayout fillLayout = new FillLayout();
		fillLayout.type = LAYOUT1_ORIENTATION.getValue();
		swtComposite.setLayout(fillLayout);
		
		swtShell.open();
	}
	
	protected void testModelSetMultipleLayout (boolean before) {
		testModelRules(before, fillLayoutRule1, fillLayoutRule2, "setMultiple");
	}
	
	protected void testModelMoveLayout(boolean before) {
		testModelRules(before, fillLayoutRule1, fillLayoutRule2, "MoveFirstToSecond");
	}
	
	protected void testModelRemoveLayoutByRemove (boolean before){
		testModelRules(before, fillLayoutRule1, fillLayoutRule2, "RemoveByRemove");
	}
	
	protected void testModelRemoveLayoutByRename (boolean before){
		testModelRules(before, fillLayoutRule1, fillLayoutRule2, "RemoveByRename");
	} 

}
