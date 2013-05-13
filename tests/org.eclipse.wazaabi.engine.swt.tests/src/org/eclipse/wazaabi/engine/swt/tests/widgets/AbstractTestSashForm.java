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
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.tests.AbstractCommandTest;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;

public abstract class AbstractTestSashForm extends AbstractCommandTest {
	protected PushButton wazButton1;
	protected PushButton wazButton2;
	protected Label wazLabel;
	
	protected org.eclipse.swt.widgets.Button swtButton1;
	protected org.eclipse.swt.widgets.Button swtButton2;
	protected org.eclipse.swt.widgets.Label swtLabel;
	
	final protected String TEXT1 = "Euranova";
	final protected String TEXT2 = "Rulezz the world";
	final protected String TEXT3 = "Don't worry";
	
	
	public void createWazaabiSashForm (boolean before, Orientation orientation) {
		
		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		
		if (!before)
			viewer.setContents(container);
		
		SashFormLayoutRule sashLayout = CoreStylesFactory.eINSTANCE.createSashFormLayoutRule();
		sashLayout.setPropertyName("layout");
		container.getStyleRules().add(sashLayout);
		
		OrientationRule orientationRule = CoreStylesFactory.eINSTANCE.createOrientationRule();
		orientationRule.setPropertyName("orientation");
		orientationRule.setValue(orientation);
		container.getStyleRules().add(orientationRule);
		
		// create a pushButton
		wazButton1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		wazButton1.setText(TEXT1); //$NON-NLS-1$

		wazButton2 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		wazButton2.setText(TEXT2); //$NON-NLS-1$
		
		wazLabel = CoreWidgetsFactory.eINSTANCE.createLabel();
		wazLabel.setText(TEXT3);
		
		// append the button to the container's children list.
		container.getChildren().add(wazButton1);
		container.getChildren().add(wazButton2);
		container.getChildren().add(wazLabel);
		
		if (before)
			viewer.setContents(container);
		
		mainShell.open();
		
	}
	
	public void createSWTSashForm (int style) {
		mainShell = new Shell (display);
		mainShell.setSize (INITIAL_WIDTH, INITIAL_HEIGHT);
		mainShell.setText("SWT");
		mainShell.setLayout (new FillLayout());

		SashForm form = new SashForm(mainShell, style);
		
		swtButton1 = new org.eclipse.swt.widgets.Button(form, SWT.PUSH);
		swtButton1.setText(TEXT1);
		
		swtButton2 = new org.eclipse.swt.widgets.Button(form, SWT.PUSH);
		swtButton2.setText(TEXT2);
		
		swtLabel = new org.eclipse.swt.widgets.Label(form, SWT.NONE);
		swtLabel.setText(TEXT3);
		
		form.setWeights(new int[] {30,30,30});
		
		mainShell.open();
	}
}
