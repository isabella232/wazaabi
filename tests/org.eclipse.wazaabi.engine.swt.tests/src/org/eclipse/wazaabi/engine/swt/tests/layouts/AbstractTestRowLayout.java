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

import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public abstract class AbstractTestRowLayout extends AbstractTestLayout {
	
	private static final int LAYOUT1_MARGIN_TOP_INITIAL_VALUE = 50;
	private static final int LAYOUT2_MARGIN_TOP_INITIAL_VALUE = 20;

	protected RowLayoutRule rowLayoutRule1; 
	protected RowLayoutRule rowLayoutRule2;
	
	@Override
	public void before() {
		super.before();
		rowLayoutRule1 = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
		rowLayoutRule1.setPropertyName("layout"); //$NON-NLS-1$
		rowLayoutRule1.setMarginTop(LAYOUT1_MARGIN_TOP_INITIAL_VALUE);
		
		rowLayoutRule2 = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
		rowLayoutRule2.setPropertyName("layout"); //$NON-NLS-1$
		rowLayoutRule2.setMarginTop(LAYOUT2_MARGIN_TOP_INITIAL_VALUE);
		
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
	}
	
	
	public void createWazaabiRowLayout(boolean before, String layoutData) {
		createWazaabiLayout(before, layoutData, rowLayoutRule1);
	}
	
	public void createWazaabiRowLayoutAndRemoveButtonTest(boolean before, String layoutData) {
		createWazaabiLayoutAndRemoveButtonTest(before, layoutData, rowLayoutRule1);
	}
	
	public void createSWTRowLayoutOneButton(String layoutData) {
		createSWTWidgetOneButton();
		
		RowLayout swtRowLayout = new RowLayout();
		swtRowLayout.marginTop = LAYOUT1_MARGIN_TOP_INITIAL_VALUE;
		swtComposite.setLayout(swtRowLayout);
		
		if (layoutData.equalsIgnoreCase("RowData")) {
			RowData swtRowData1 = new RowData();
			swtButton1.setLayoutData(swtRowData1);
			swtRowData1.height = BUTTON1_HEIGHT;
			swtRowData1.width = BUTTON1_WIDTH;
		}
		
		swtShell.open();
	}
	
	public void createSWTRowLayoutTwoButtons(String layoutData) {
		createSWTWidgetTwoButtons();
		
		RowLayout swtRowLayout = new RowLayout();
		swtRowLayout.marginTop = LAYOUT1_MARGIN_TOP_INITIAL_VALUE;
		swtComposite.setLayout(swtRowLayout);
		
		if (layoutData.equalsIgnoreCase("RowData")) {
			RowData swtRowData1 = new RowData();
			swtButton1.setLayoutData(swtRowData1);
			swtRowData1.height = BUTTON1_HEIGHT;
			swtRowData1.width = BUTTON1_WIDTH;

			RowData swtRowData2 = new RowData();
			swtButton2.setLayoutData(swtRowData2);
			swtRowData2.height = BUTTON2_HEIGHT;
			swtRowData2.width = BUTTON2_WIDTH;
		}
		
		swtShell.open();
	}
	
	protected void testModelSetMultipleLayout (boolean before) {
		testModelRules(before, rowLayoutRule1, rowLayoutRule2, "setMultiple");
	}
	
	protected void testModelMoveLayout(boolean before) {
		testModelRules(before, rowLayoutRule1, rowLayoutRule2, "MoveFirstToSecond");
	}
	
	protected void testModelRemoveLayoutByRemove (boolean before){
		testModelRules(before, rowLayoutRule1, rowLayoutRule2, "RemoveByRemove");
	}
	
	protected void testModelRemoveLayoutByRename (boolean before){
		testModelRules(before, rowLayoutRule1, rowLayoutRule2, "RemoveByRename");
	} 

}
