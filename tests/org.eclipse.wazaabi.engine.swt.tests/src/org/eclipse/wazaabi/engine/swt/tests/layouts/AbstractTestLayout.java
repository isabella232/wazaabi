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
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.tests.AbstractCommandTest;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
import org.junit.Assert;

public abstract class AbstractTestLayout extends AbstractCommandTest {

	protected static final String BUTTON1_TEXT = "Hello world";
	protected static final String BUTTON2_TEXT = "This is wazaabi tests";

	protected final int BUTTON1_WIDTH = 100;
	protected final int BUTTON1_HEIGHT = 50;
	protected final int BUTTON2_WIDTH = 20;
	protected final int BUTTON2_HEIGHT = 10;
	
	protected Container container = null;
	protected Composite swtComposite = null;
	
	protected Shell swtShell = null;
	
	protected PushButton button1 = null;
	protected PushButton button2 = null;
	
	protected Button swtButton1 = null;
	protected Button swtButton2 = null;
	
	@Override
	public void before() {
		super.before();
		Assert.assertNull(container);
		Assert.assertNull(button1);
		Assert.assertNull(button2);
	};
	
	@Override
	public void after() {
		container = null;
		button1 = null;
		button2 = null;
		super.after();
	}
	
	public void addButtons(boolean before, String layoutData) {
		if (!before) {
			// Set the content
			getViewer().setContents(container);
		}
		
		if (layoutData.equalsIgnoreCase("RowData")) {
			addButtonsWithRowData();
		} else if (layoutData.equalsIgnoreCase("GridData")) {
			addButtonsWithGridData();
		} else {
			addButtons();
		}
		
		if (before) {
			// Set the content
			getViewer().setContents(container);
		}
		
	}

	public void addButtons() {
		// create the first PushButton and add it to container
		button1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		container.getChildren().add(button1);
		button1.setText(BUTTON1_TEXT);

		// create the second PushButton and add it to container
		button2 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button2.setText(BUTTON2_TEXT);
		container.getChildren().add(button2);
	}
	
	public void addButtonsWithRowData () {
		addButtons();
		
		RowDataRule rowDataRule1 = SWTStylesFactory.eINSTANCE.createRowDataRule();
		button1.getStyleRules().add(rowDataRule1);
		rowDataRule1.setPropertyName("layout-data"); //$NON-NLS-1$
		rowDataRule1.setHeight(BUTTON1_HEIGHT);
		rowDataRule1.setWidth(BUTTON1_WIDTH);

		RowDataRule rowDataRule2 = SWTStylesFactory.eINSTANCE.createRowDataRule();
		rowDataRule2.setPropertyName("layout-data"); //$NON-NLS-1$
		rowDataRule2.setHeight(BUTTON2_HEIGHT);
		rowDataRule2.setWidth(BUTTON2_WIDTH);
		button2.getStyleRules().add(rowDataRule2);
	}
	
	public void addButtonsWithGridData () {
		addButtons();
		
		GridDataRule gridDataRule1 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		button1.getStyleRules().add(gridDataRule1);
		gridDataRule1.setPropertyName("layout-data"); //$NON-NLS-1$
		gridDataRule1.setHorizontalAlignement(GridDataAlignment.CENTER);

		GridDataRule gridDataRule2 = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gridDataRule2.setPropertyName("layout-data"); //$NON-NLS-1$
		gridDataRule2.setHorizontalAlignement(GridDataAlignment.END);
		button2.getStyleRules().add(gridDataRule2);
	}
	
	public void createSWTWidgetOneButton() {
		// create the shell which will receive the pure SWT components
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);

		// create the content
		swtComposite = new Composite(swtShell, SWT.NONE);
		
		swtButton1 = new Button(swtComposite, SWT.PUSH);
		swtButton1.setText(BUTTON1_TEXT);
	}
	
	public void createSWTWidgetTwoButtons() {
		createSWTWidgetOneButton();
		swtButton2 = new Button(swtComposite, SWT.PUSH);
		swtButton2.setText(BUTTON2_TEXT);
	}
	
	public void createWazaabiLayout(boolean before, String layoutData, StyleRule layout) {
		if (before) {
			container.getStyleRules().add(layout);
		}
		addButtons(before, layoutData);
		if (!before) {
			container.getStyleRules().add(layout);
		}
		getMainShell().open();
	}
	
	public void createWazaabiLayoutAndRemoveButtonTest(boolean before, String layoutData, StyleRule layout) {
		container.getStyleRules().add(layout);
		addButtons(before, layoutData);
		container.getChildren().remove(button2);
		getMainShell().open();
	}
	
	
	protected void testModelRules (boolean before, StyleRule rule1, StyleRule rule2, String action) {
		if (!before) {
			// Set the content
			getViewer().setContents(container);
		}
		
		if (action.equalsIgnoreCase("SetMultiple")) {
			container.getStyleRules().add(rule1);	
			container.getStyleRules().add(rule2);
		} else if (action.equalsIgnoreCase("MoveFirstToSecond")) {
			container.getStyleRules().add(rule2);	
			container.getStyleRules().add(rule1);
			container = (Container) TestUtils.switchFirstAndSecondRule(container,rule2.getPropertyName());
		} else if (action.equalsIgnoreCase("RemoveByRemove")) {
			container.getStyleRules().add(rule2);	
			container.getStyleRules().add(rule1);
			container = (Container) TestUtils.removeFirstRuleByRemove(container,rule2.getPropertyName());
		} else if (action.equalsIgnoreCase("RemoveByRename")) {
			container.getStyleRules().add(rule2);	
			container.getStyleRules().add(rule1);
			container = (Container) TestUtils.removeFirstRuleByRename(container,rule2.getPropertyName());
		} else {
			Assert.assertNotNull(null);
		}

		addButtons();
		
		if (before) {
			// Set the content
			getViewer().setContents(container);
		}
		getMainShell().open();
	}
}
