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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
import org.junit.Assert;

public abstract class AbstractTestLayoutInALayout extends AbstractTestLayout {

	private static final String BUTTON1_TEXT = "Hello world";
	private static final String BUTTON2_TEXT = "This is wazaabi tests";
	private static final String LABEL_TEXT = "Layout in a layout";
	
	private static final Orientation LAYOUT_ORIENTATION = Orientation.HORIZONTAL;

	Label label = null;

	Shell swtShell = null;
	org.eclipse.swt.widgets.Label swtLabel = null;

	Button swtButton1 = null;
	Button swtButton2 = null;


	@Override
	public void after() {
		Assert.assertNotNull(getDisplay());
		Assert.assertTrue(!getDisplay().isDisposed());
		getDisplay().dispose();
		setDisplay(null);
	}


	protected void createSWTWidget(String parentLayout, String childLayout) {
		
		// create the shell which will receive the pure SWT components
		swtShell = new Shell(getDisplay(),SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);

		// create the content
		Composite swtComposite1 = new Composite(swtShell, SWT.NONE);
		if (parentLayout.equals("RowLayout")) {
			swtComposite1.setLayout(new RowLayout());
		} else if (parentLayout.equals("GridLayout")) {
			swtComposite1.setLayout(new GridLayout());
		} else if (parentLayout.equals("FillLayout")) {
			swtComposite1.setLayout(new FillLayout());
		} else {
			Assert.assertNotNull(null);
		}

		swtLabel = new org.eclipse.swt.widgets.Label(swtComposite1, SWT.NONE);
		swtLabel.setText(LABEL_TEXT);

		Composite swtComposite2 = new Composite(swtComposite1, SWT.NONE);
		if (childLayout.equals("RowLayout")) {
			swtComposite2.setLayout(new RowLayout());
		} else if (childLayout.equals("GridLayout")) {
			swtComposite2.setLayout(new GridLayout());
		} else if (childLayout.equals("FillLayout")) {
			swtComposite2.setLayout(new FillLayout());
		} else {
			Assert.assertNotNull(null);
		}

		swtButton1 = new Button(swtComposite2, SWT.PUSH);
		swtButton1.setText(BUTTON1_TEXT);
		swtButton2 = new Button(swtComposite2, SWT.PUSH);
		swtButton2.setText(BUTTON2_TEXT);
		swtButton2.setText(swtButton2.getText());		
		
		swtShell.layout(false, true);

		swtShell.open();
	}
	
	public void createWazaabiWidget (boolean before, String parentLayout, String childLayout) {
		Container container1 = CoreWidgetsFactory.eINSTANCE.createContainer();
		Container container2 = CoreWidgetsFactory.eINSTANCE.createContainer();
		label = CoreWidgetsFactory.eINSTANCE.createLabel();
		label.setText(LABEL_TEXT);
		button1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button2 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		
		
		if (!before) {
			getViewer().setContents(container1);
		}
		
		container1.getChildren().add(label);
		container1.getChildren().add(container2);
		
		if (parentLayout.equals("GridLayout")) {
			GridLayoutRule gridLayoutRule = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
			gridLayoutRule.setPropertyName("layout");
			container1.getStyleRules().add(gridLayoutRule);
		} else if (parentLayout.equals("RowLayout")) {
			RowLayoutRule rowLayoutRule = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
			rowLayoutRule.setPropertyName("layout"); //$NON-NLS-1$
			//rowLayoutRule.setType(LAYOUT_ORIENTATION);
			container1.getStyleRules().add(rowLayoutRule);
		} else if (parentLayout.equals("FillLayout")) {
			FillLayoutRule fillLayoutRule = SWTStylesFactory.eINSTANCE.createFillLayoutRule();
			fillLayoutRule.setPropertyName("layout"); //$NON-NLS-1$
			fillLayoutRule.setType(LAYOUT_ORIENTATION);
			container1.getStyleRules().add(fillLayoutRule);
		} else {
			Assert.assertNotNull(null);
		}
		
		//addButtons(before, "");
		
		container2.getChildren().add(button1);
		button1.setText(BUTTON1_TEXT);

		// create the second PushButton and add it to container
		
		button2.setText(BUTTON2_TEXT);
		container2.getChildren().add(button2);
		
		if (childLayout.equals("GridLayout")) {
			GridLayoutRule gridLayoutRule = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
			gridLayoutRule.setPropertyName("layout");
			container2.getStyleRules().add(gridLayoutRule);
		} else if (childLayout.equals("RowLayout")) {
			RowLayoutRule rowLayoutRule = SWTStylesFactory.eINSTANCE.createRowLayoutRule();
			rowLayoutRule.setPropertyName("layout"); //$NON-NLS-1$
			//rowLayoutRule.setType(LAYOUT_ORIENTATION);
			container2.getStyleRules().add(rowLayoutRule);
		} else if (childLayout.equals("FillLayout")) {
			FillLayoutRule fillLayoutRule = SWTStylesFactory.eINSTANCE.createFillLayoutRule();
			fillLayoutRule.setPropertyName("layout"); //$NON-NLS-1$
			fillLayoutRule.setType(LAYOUT_ORIENTATION);
			container2.getStyleRules().add(fillLayoutRule);
		} else {
			Assert.assertNotNull(null);
		}

		
		if (before) {
			getViewer().setContents(container1);
		}

		getMainShell().open();
	}



}
