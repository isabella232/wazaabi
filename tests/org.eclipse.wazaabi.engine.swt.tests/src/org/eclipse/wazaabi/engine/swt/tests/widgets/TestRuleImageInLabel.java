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

import junit.framework.Assert;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.tests.AbstractCommandTest;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
import org.junit.Test;

public class TestRuleImageInLabel extends AbstractCommandTest {

	private static final String URI1 = "urn:java:Idea.jpg"; //$NON-NLS-1$
	private static final String URI2 = "urn:java:download.png"; //$NON-NLS-1$
	
	private Image firstSWTImage;
	private Image secondSWTImage;
	
	//private PushButton pushButton = null;
	
	@Override
	public void before() {
		
		// init SWT Engine in standalone mode
		SWTHelper.init();
		// init the 'urn:java' resolver
		URNJavaLocatorHelper.init();
		
		// create the display
		Assert.assertNull(getDisplay());
		setDisplay(new Display());
		
		Assert.assertNotNull(display);
		Assert.assertTrue(!display.isDisposed());
		
		// create the shell which will receive the wazaabi components
		mainShell = new Shell(display, SWT.SHELL_TRIM);
		Assert.assertNotNull(mainShell);
		Assert.assertFalse(mainShell.isDisposed());
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		// create the viewer
		viewer = new SWTControlViewer(mainShell);
		Assert.assertNotNull(viewer);

		// create the PushButton
		//pushButton = WidgetsFactory.eINSTANCE.createPushButton();
	
	}
	
	@Override
	public void after() {
		
		Assert.assertNotNull(getDisplay());
		Assert.assertFalse(getDisplay().isDisposed());
		getDisplay().dispose();
		setDisplay(null);

		// the last label image has been disposed
		Assert.assertTrue(secondSWTImage.isDisposed());
	}

	/**
	 * Test whether a given SWT label image (based on uri1) is well disposed
	 * when the URI property is set to uri2 when viewer setContents has been
	 * made.
	 * 
	 * @throws Exception
	 */
	@Test
	public void testSWTImageDisposedWhenImageRuleURIChangedAfterViewerSetContents() {
		
		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();

		FillLayoutRule fillLayoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		fillLayoutRule.setPropertyName("layout"); //$NON-NLS-1$
		container.getStyleRules().add(fillLayoutRule);

		// create the label
		Label label = CoreWidgetsFactory.eINSTANCE.createLabel();

		// create the Image
		ImageRule imageRule = CoreStylesFactory.eINSTANCE
				.createImageRule();
		imageRule.setPropertyName("image"); //$NON-NLS-1$
		imageRule.setValue(URI1);

		label.getStyleRules().add(imageRule);

		container.getChildren().add(label);

		// Set the content
		viewer.setContents(container);
		mainShell.open();

		org.eclipse.swt.widgets.Label swtLabel = (org.eclipse.swt.widgets.Label) SWTUtils
				.getWidget(viewer, label);
		firstSWTImage = swtLabel.getImage();
		// the current label image is not null
		Assert.assertNotNull(firstSWTImage);
		// the current label image is not disposed
		Assert.assertFalse(firstSWTImage.isDisposed());

		imageRule.setValue(URI2);

		// get the current label image == second one
		secondSWTImage = swtLabel.getImage();
		// the current label image is not null
		Assert.assertNotNull(secondSWTImage);
		// the current label image is not disposed
		Assert.assertFalse(secondSWTImage.isDisposed());

		// the previous label image has been disposed
		Assert.assertTrue(firstSWTImage.isDisposed());

	}

}
