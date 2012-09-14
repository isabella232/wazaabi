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

import org.eclipse.wazaabi.engine.swt.tests.AbstractCommandTest;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.junit.Assert;
import org.junit.Test;

public class TestLabel extends AbstractCommandTest {
	
	private static final String TEXT = "Hello World"; //$NON-NLS-1$
	private Label label;


	
	@Override
	public void before() {
		super.before();
		// create the PushButton
		label = CoreWidgetsFactory.eINSTANCE.createLabel();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	

	@Test
	public void testModelLabelTextSetBeforeViewerSetContentsEqualsSWTButtonText() {

		// set the PushButton's text
		StringRule textRule = CoreStylesFactory.eINSTANCE.createStringRule();
		textRule.setPropertyName("text"); //$NON-NLS-1$
		textRule.setValue(TEXT);
		label.getStyleRules().add(textRule);

		// render the content
		viewer.setContents(label);

		org.eclipse.swt.widgets.Label swtLabel = (org.eclipse.swt.widgets.Label) SWTUtils.getWidget(viewer, label);
		Assert.assertEquals(TEXT, swtLabel.getText());
	}

	@Test
	public void testModelLabelTextSetAfterViewerSetContentsEqualsSWTButtonText() {

		// Set the content
		viewer.setContents(label);

		org.eclipse.swt.widgets.Label swtLabel = (org.eclipse.swt.widgets.Label) SWTUtils.getWidget(viewer, label);

		// set the PushButton's text
		StringRule textRule = CoreStylesFactory.eINSTANCE.createStringRule();
		textRule.setPropertyName("text"); //$NON-NLS-1$
		textRule.setValue(TEXT);
		label.getStyleRules().add(textRule);

		Assert.assertEquals(TEXT, swtLabel.getText());

	}
}
