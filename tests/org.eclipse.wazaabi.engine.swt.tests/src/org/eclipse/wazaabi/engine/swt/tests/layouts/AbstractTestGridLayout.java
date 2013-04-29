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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public abstract class AbstractTestGridLayout extends AbstractTestLayout {

	private static final int LAYOUT1_NUM_COLS = 2;
	private static final int LAYOUT2_NUM_COLS = 5;
	
	private static final int LAYOUT1_HORIZONTAL_SPACING = 1;
	private static final int LAYOUT2_HORIZONTAL_SPACING = 10;

	protected GridLayoutRule gridLayoutRule1; 
	protected GridLayoutRule gridLayoutRule2;
	
	@Override
	public void before() {
		super.before();
		gridLayoutRule1 = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
		gridLayoutRule1.setPropertyName("layout"); //$NON-NLS-1$
		gridLayoutRule1.setNumColumns(LAYOUT1_NUM_COLS);
		gridLayoutRule1.setHorizontalSpacing(LAYOUT1_HORIZONTAL_SPACING);
		
		gridLayoutRule2 = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
		gridLayoutRule2.setPropertyName("layout"); //$NON-NLS-1$
		gridLayoutRule2.setNumColumns(LAYOUT2_NUM_COLS);
		gridLayoutRule2.setHorizontalSpacing(LAYOUT2_HORIZONTAL_SPACING);
		
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
	}

	public void createWazaabiGridLayout(boolean before, String layoutData) {
		createWazaabiLayout(before, layoutData, gridLayoutRule1);
	}
	
	public void createWazaabiGridLayoutAndRemoveButtonTest(boolean before, String layoutData) {
		createWazaabiLayoutAndRemoveButtonTest(before, layoutData, gridLayoutRule1);
	}
	
	public void createSWTGridLayoutOneButton(String layoutData) {
		createSWTWidgetOneButton();
		
		GridLayout swtGridLayout = new GridLayout();
		swtGridLayout.numColumns = LAYOUT1_NUM_COLS;
		swtGridLayout.horizontalSpacing = LAYOUT1_HORIZONTAL_SPACING;
		swtComposite.setLayout(swtGridLayout);
		

		if (layoutData.equalsIgnoreCase("GridData")) {
			GridData swtGridData1 = new GridData();
			swtButton1.setLayoutData(swtGridData1);
			swtGridData1.horizontalAlignment = SWT.CENTER;
		}
		
		swtShell.open();
	}
	
	public void createSWTGridLayoutTwoButtons(String layoutData) {
		createSWTWidgetTwoButtons();
		
		GridLayout swtGridLayout = new GridLayout();
		swtGridLayout.numColumns = LAYOUT1_NUM_COLS;
		swtGridLayout.horizontalSpacing = LAYOUT1_HORIZONTAL_SPACING;
		swtComposite.setLayout(swtGridLayout);
		

		if (layoutData.equalsIgnoreCase("GridData")) {
			GridData swtGridData1 = new GridData();
			swtButton1.setLayoutData(swtGridData1);
			swtGridData1.horizontalAlignment = SWT.CENTER;

			GridData swtGridData2 = new GridData();
			swtButton2.setLayoutData(swtGridData2);
			swtGridData2.horizontalAlignment = SWT.LEFT;
		}
		
		swtShell.open();
	}

	
	
	protected void testModelSetMultipleLayout (boolean before) {
		testModelRules(before, gridLayoutRule1, gridLayoutRule2, "setMultiple");
	}
	
	protected void testModelMoveLayout(boolean before) {
		testModelRules(before, gridLayoutRule1, gridLayoutRule2, "MoveFirstToSecond");
	}
	
	protected void testModelRemoveLayoutByRemove (boolean before){
		testModelRules(before, gridLayoutRule1, gridLayoutRule2, "RemoveByRemove");
	}
	
	protected void testModelRemoveLayoutByRename (boolean before){
		testModelRules(before, gridLayoutRule1, gridLayoutRule2, "RemoveByRename");
	} 
}
