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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.CoolItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BarLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Scale;

public class AbstractTestBarLayout extends AbstractTestLayout {
	
	protected BarLayoutRule coolBarLayoutRule; 
	protected BarLayoutRule toolBarLayoutRule;
	
	protected Scale scale = null;
	
	protected org.eclipse.swt.widgets.Scale swtScale = null;
	
	@Override
	public void before() {
		super.before();
		coolBarLayoutRule = CoreStylesFactory.eINSTANCE.createBarLayoutRule();
		coolBarLayoutRule.setPropertyName("layout");
		coolBarLayoutRule.setDraggable(true);
		
		toolBarLayoutRule = CoreStylesFactory.eINSTANCE.createBarLayoutRule();
		toolBarLayoutRule.setPropertyName("layout");
		toolBarLayoutRule.setDraggable(false);
		
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
	}

	public void createWazaabiBarLayout(boolean before, String type, Orientation orientation) {
		OrientationRule orientationRule = CoreStylesFactory.eINSTANCE.createOrientationRule();
		orientationRule.setPropertyName("orientation");
		orientationRule.setValue(orientation);		
		if (!before && orientation != null)
			container.getStyleRules().add(orientationRule);
		if ("coolbar".equalsIgnoreCase(type))
			createWazaabiLayout(before, "", coolBarLayoutRule);
		else
			createWazaabiLayout(before, "", toolBarLayoutRule);
		if (before && orientation != null)
			container.getStyleRules().add(orientationRule);
	}
	
	public void createWaraabiBarLayoutWithDifferentWidgets(boolean before, String type) {
		if (before)
			viewer.setContents(container);
		
		if ("coolbar".equalsIgnoreCase(type))
			container.getStyleRules().add(coolBarLayoutRule);
		else
			container.getStyleRules().add(toolBarLayoutRule);
		
		// create the first PushButton and add it to container
		button1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		container.getChildren().add(button1);
		button1.setText(BUTTON1_TEXT);

		// create the second PushButton and add it to container
		scale = CoreWidgetsFactory.eINSTANCE.createScale();
		scale.setMaximum(200);
		scale.setMinimum(0);
		scale.setPageIncrement(50);
		container.getChildren().add(scale);
		
		if (!before)
			viewer.setContents(container);
	}

	
	public void createWazaabiBarLayoutAndRemoveButtonTest(boolean before) {
		createWazaabiLayoutAndRemoveButtonTest(before, "", coolBarLayoutRule);
	}
	
	public void createSWTToolBarLayoutWithDifferentWidgets() {
		// create the shell which will receive the pure SWT components
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		// create the content
		ToolBar swtToolBar = new ToolBar(swtShell, SWT.NONE);
		
		swtButton1 = new Button(swtToolBar, SWT.PUSH);
		swtButton1.setText(BUTTON1_TEXT);
		Point size1 = swtButton1.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		ToolItem item1 = new ToolItem(swtToolBar, SWT.SEPARATOR);
		item1.setControl(swtButton1);
		item1.setWidth(size1.x);
		
		swtScale = new org.eclipse.swt.widgets.Scale(swtToolBar, SWT.PUSH);
		swtScale.setMaximum(200);
		swtScale.setMinimum(0);
		swtScale.setPageIncrement(50);
		Point size2 = swtScale.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		ToolItem item2 = new ToolItem(swtToolBar, SWT.SEPARATOR);
		item2.setControl(swtScale);
		item2.setWidth(size2.x);

		swtShell.open();
	}

	
	public void createSWTToolBar() {
		// create the shell which will receive the pure SWT components
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		// create the content
		ToolBar swtToolBar = new ToolBar(swtShell, SWT.NONE);
		
		swtButton1 = new Button(swtToolBar, SWT.PUSH);
		swtButton1.setText(BUTTON1_TEXT);
		Point size1 = swtButton1.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		ToolItem item1 = new ToolItem(swtToolBar, SWT.SEPARATOR);
		item1.setControl(swtButton1);
		item1.setWidth(size1.x);
		
		swtButton2 = new Button(swtToolBar, SWT.PUSH);
		swtButton2.setText(BUTTON2_TEXT);
		Point size2 = swtButton2.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		ToolItem item2 = new ToolItem(swtToolBar, SWT.SEPARATOR);
		item2.setControl(swtButton2);
		item2.setWidth(size2.x);

		swtShell.open();
		
	}
	
	public void createSWTCoolBarLayoutWithDifferentWidgets() {
		// create the shell which will receive the pure SWT components
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		// create the content
		CoolBar swtCoolBar = new CoolBar(swtShell, SWT.NONE);
		
		swtButton1 = new Button(swtCoolBar, SWT.PUSH);
		swtButton1.setText(BUTTON1_TEXT);
		Point size1 = swtButton1.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		CoolItem item1 = new CoolItem(swtCoolBar, SWT.SEPARATOR);
		item1.setControl(swtButton1);
		item1.setPreferredSize(item1.computeSize(size1.x, size1.y));

		
		swtScale = new org.eclipse.swt.widgets.Scale(swtCoolBar, SWT.PUSH);
		swtScale.setMaximum(200);
		swtScale.setMinimum(0);
		swtScale.setPageIncrement(50);
		Point size2 = swtScale.computeSize (SWT.DEFAULT, SWT.DEFAULT);
		CoolItem item2 = new CoolItem(swtCoolBar, SWT.SEPARATOR);
		item2.setControl(swtScale);
		item2.setPreferredSize(item2.computeSize(size2.x, size2.y));

		swtShell.open();

	}
	
	public void createSWTCoolBar() {
		// create the shell which will receive the pure SWT components
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		// create the content
		CoolBar swtCoolBar = new CoolBar(swtShell, SWT.NONE);
		
		swtButton1 = new Button(swtCoolBar, SWT.PUSH);
		swtButton1.setText(BUTTON1_TEXT);
		Point size1 = swtButton1.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		CoolItem item1 = new CoolItem(swtCoolBar, SWT.SEPARATOR);
		item1.setControl(swtButton1);
		item1.setPreferredSize(item1.computeSize(size1.x, size1.y));

		
		swtButton2 = new Button(swtCoolBar, SWT.PUSH);
		swtButton2.setText(BUTTON2_TEXT);
		Point size2 = swtButton2.computeSize (SWT.DEFAULT, SWT.DEFAULT);
		CoolItem item2 = new CoolItem(swtCoolBar, SWT.SEPARATOR);
		item2.setControl(swtButton2);
		item2.setPreferredSize(item2.computeSize(size2.x, size2.y));

		swtShell.open();

	}
	
	protected void convertBarLayout(String type) {
		if ("coolbar".equalsIgnoreCase(type)) {
			toolBarLayoutRule.setDraggable(true);
		} else {
			coolBarLayoutRule.setDraggable(false);
		}
	}
	

	protected void testModelSetMultipleLayout (boolean before, String type) {
		if ("coolbar".equalsIgnoreCase(type))
			testModelRules(before, coolBarLayoutRule, toolBarLayoutRule, "setMultiple");
		else
			testModelRules(before, toolBarLayoutRule, coolBarLayoutRule, "setMultiple");
	}
	
	protected void testModelMoveLayout(boolean before, String type) {
		if ("coolbar".equalsIgnoreCase(type))
			testModelRules(before, coolBarLayoutRule, toolBarLayoutRule, "MoveFirstToSecond");
		else
			testModelRules(before, toolBarLayoutRule, coolBarLayoutRule, "MoveFirstToSecond");
	}
	
	protected void testModelRemoveLayoutByRemove (boolean before, String type){
		if ("coolbar".equalsIgnoreCase(type))
			testModelRules(before, coolBarLayoutRule, toolBarLayoutRule, "RemoveByRemove");
		else
			testModelRules(before, toolBarLayoutRule, coolBarLayoutRule, "RemoveByRemove");
	}
	
	protected void testModelRemoveLayoutByRename (boolean before, String type){
		if ("coolbar".equalsIgnoreCase(type))
			testModelRules(before, coolBarLayoutRule, toolBarLayoutRule, "RemoveByRename");
		else
			testModelRules(before, toolBarLayoutRule, coolBarLayoutRule, "RemoveByRename");
	} 
}
