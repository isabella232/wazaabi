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
import org.eclipse.swt.widgets.Label;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Assert;
import org.junit.Test;

public class TestImageRuleImageLabel extends AbstractTestImageRuleImage{
	
	private org.eclipse.wazaabi.mm.core.widgets.Label widget;
	
	@Override
	public void before() {
		super.before();
		widget = CoreWidgetsFactory.eINSTANCE.createLabel();
	}
	
	@Override
	public void after() {
		mainShell.open();
		super.after();
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wazaabi.engine.swt.commons.tests.widgets.AbstractTestStyleRule#ModelMoveRule(boolean, org.eclipse.wazaabi.mm.core.widgets.Widget, org.eclipse.wazaabi.mm.core.styles.StyleRule, org.eclipse.wazaabi.mm.core.styles.StyleRule, java.lang.String)
	 * The following methods override those from AbtractTestStyleRules in order to be able to test that images are well disposed.
	 */
	@Override
	protected org.eclipse.swt.widgets.Widget ModelMoveRule(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName) {
		if(!before)
			viewer.setContents(widget);
		
		Image firstSwtImage=null;
		widget.getStyleRules().add(styleRule1);

		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtLabel.getImage();
			Assert.assertNotNull(firstSwtImage);
			Assert.assertFalse(firstSwtImage.isDisposed());
		}
		
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.switchFirstAndSecondRule(widget, propertyName);
		
		if (!before) {
			Assert.assertTrue(firstSwtImage.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
				
		return SWTUtils.getWidget(viewer,widget); 
		
	}
	
	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRemove(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		Image firstSwtImage=null;
		widget.getStyleRules().add(styleRule1);
		
		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtLabel.getImage();
			Assert.assertNotNull(firstSwtImage);
			Assert.assertFalse(firstSwtImage.isDisposed());
		}
		
		widget.getStyleRules().add(styleRule2);
		widget = TestUtils.removeFirstRuleByRemove(widget,propertyName);
		
		if (!before) {
			Assert.assertTrue(firstSwtImage.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Override
	protected org.eclipse.swt.widgets.Widget ModelRemoveRuleByRename(boolean before, Widget widget, StyleRule styleRule1, StyleRule styleRule2, String propertyName){
		if(!before)
			viewer.setContents(widget);
		
		Image firstSwtImage=null;
		widget.getStyleRules().add(styleRule1);
		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtLabel.getImage();
			Assert.assertNotNull(firstSwtImage);
			Assert.assertFalse(firstSwtImage.isDisposed());
		}
		widget.getStyleRules().add(styleRule2);
		widget =  TestUtils.removeFirstRuleByRename(widget,propertyName);
		
		if (!before) {
			Assert.assertTrue(firstSwtImage.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
	
	} 
	
	protected org.eclipse.swt.widgets.Widget ModelModifyRule(boolean before, Widget widget){
		if(!before)
			viewer.setContents(widget);
		
		Image firstSwtImage=null;
		widget.getStyleRules().add(imageRuleImage);
		
		if(!before){
			Label swtLabel = (Label) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtLabel.getImage();
			Assert.assertNotNull(firstSwtImage);
			Assert.assertFalse(firstSwtImage.isDisposed());
		}
		
		StringRule str =(ImageRule) widget.getStyleRules().get(0);
		str.setValue(URI2);
		
		if (!before) {
			Assert.assertTrue(firstSwtImage.isDisposed());
		}
		
		if(before)
			viewer.setContents(widget);
		
		return SWTUtils.getWidget(viewer, widget);
		
	}
	
	@Test
	public void testModelSetImageBeforeViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelSetRule(true, widget, imageRuleImage);
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelSetImageAfterViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelSetRule(false, widget, imageRuleImage);
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelSetMultipleImageBeforeViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelSetMultipleRule(true, widget, imageRuleImage, imageRuleImage2);
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelSetMultipleImageAfterViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelSetMultipleRule(false, widget, imageRuleImage, imageRuleImage2);
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image1.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelMoveImageBeforeViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelMoveRule(true, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelMoveImageAfterViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelMoveRule(false, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelRemoveImageByRemoveBeforeViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(true, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
		
	}
	
	@Test
	public void testModelRemoveImageByRemoveAfterViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelRemoveRuleByRemove(false, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
		
	}
	
	@Test
	public void testModelRemoveImageByRenameBeforeViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(true, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(),swtImage.getBounds());
	}
	
	@Test
	public void testModelRemoveImageByRenameAfterViewerSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelRemoveRuleByRename(false, widget, imageRuleImage, imageRuleImage2, "image");
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(),swtImage.getBounds());
	}
	
	@Test
	public void testModelModifyImageBeforeSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelModifyRule(true, widget);
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	@Test
	public void testModelModifyImageAfterSetContentsEqualsSWTImage(){
		Label swtLabel = (Label) ModelModifyRule(false, widget);
		Image swtImage = swtLabel.getImage();
		Assert.assertNotNull(swtImage);
		Assert.assertEquals(image2.getBounds(), swtImage.getBounds());
	}
	
	
	
	
	
	
}
