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

import java.io.IOException;

import org.eclipse.swt.graphics.Image;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.junit.Test;

public abstract class AbstractTestImageRuleImage extends AbstractTestStyleRule {

	protected ImageRule imageRuleImage;
	protected ImageRule imageRuleImage2;
	protected ImageRule imageRuleImage3;

	protected static final String URI1 = "urn:java:Idea.jpg"; //$NON-NLS-1$
	protected static final String URI2 = "urn:java:download.png"; //$NON-NLS-1$
	protected static final String URI3 = "urn:java:hacker.gif"; //$NON-NLS-1$

	protected Image image1;
	protected Image image2;
	protected Image image3;

	@Override
	public void before() {
		super.before();
		imageRuleImage = CoreStylesFactory.eINSTANCE.createImageRule();
		imageRuleImage.setPropertyName("image");
		imageRuleImage.setValue(URI1); //$NON-NLS-1$

		imageRuleImage2 = CoreStylesFactory.eINSTANCE.createImageRule();
		imageRuleImage2.setPropertyName("image");
		imageRuleImage2.setValue(URI2); //$NON-NLS-1$

		imageRuleImage3 = CoreStylesFactory.eINSTANCE.createImageRule();
		imageRuleImage3.setPropertyName("image");
		imageRuleImage3.setValue(URI3); //$NON-NLS-1$

		try {
			image1 = new Image(getDisplay(), EDPSingletons
					.getComposedCodeLocator().getResourceInputStream(URI1));
			image2 = new Image(getDisplay(), EDPSingletons
					.getComposedCodeLocator().getResourceInputStream(URI2));
			image3 = new Image(getDisplay(), EDPSingletons
					.getComposedCodeLocator().getResourceInputStream(URI3));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	@Override
	public void after() {
		mainShell.open();
		super.after();
	}

	@Test
	abstract public void testModelSetImageBeforeViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelSetImageAfterViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelSetMultipleImageBeforeViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelSetMultipleImageAfterViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelMoveImageBeforeViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelMoveImageAfterViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelRemoveImageByRemoveBeforeViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelRemoveImageByRemoveAfterViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelRemoveImageByRenameBeforeViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelRemoveImageByRenameAfterViewerSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelModifyImageBeforeSetContentsEqualsSWTImage();

	@Test
	abstract public void testModelModifyImageAfterSetContentsEqualsSWTImage();

}
