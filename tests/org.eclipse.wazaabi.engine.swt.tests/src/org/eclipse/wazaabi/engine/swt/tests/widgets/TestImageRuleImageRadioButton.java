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

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.RadioButton;
import org.junit.Assert;
import org.junit.Test;

public class TestImageRuleImageRadioButton extends AbstractTestImageRuleImageButton{
	
	private RadioButton widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createRadioButton();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	@Test
	public void testModelSetImageBeforeViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelSetRule(true, widget, imageRuleImage);
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelSetImageAfterViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelSetRule(false, widget, imageRuleImage);
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelSetMultipleImageBeforeViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelSetMultipleRule(true, widget, imageRuleImage, imageRuleImage2);
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelSetMultipleImageAfterViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelSetMultipleRule(false, widget, imageRuleImage, imageRuleImage2);
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelMoveImageBeforeViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelMoveRule(true, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelMoveImageAfterViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelMoveRule(false, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelRemoveImageByRemoveBeforeViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelRemoveRuleByRemove(true, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
		
	}
	
	@Test
	public void testModelRemoveImageByRemoveAfterViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelRemoveRuleByRemove(false, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelRemoveImageByRenameBeforeViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelRemoveRuleByRename(true, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(),swtImage.getBounds());
	}
	
	@Test
	public void testModelRemoveImageByRenameAfterViewerSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelRemoveRuleByRename(false, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(),swtImage.getBounds());
	}
	
	@Test
	public void testModelModifyImageBeforeSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelModifyRule(true, widget);
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelModifyImageAfterSetContentsEqualsSWTImage(){
		Button swtButton = (Button) ModelModifyRule(false, widget);
		Image swtImage = swtButton.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	
	
	
	
	
}
