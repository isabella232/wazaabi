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
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.engine.swt.tests.TestUtils;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.junit.Assert;
import org.junit.Test;

public abstract class AbstractTestImageRuleImageButton extends AbstractTestImageRuleImage {
	
	
		   
	@Override
	public void before() {
		super.before();
		
	}
	
	@Override
	public void after() {
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
			Button swtButton = (Button) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtButton.getImage();
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
			Button swtButton = (Button) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtButton.getImage();
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
			Button swtButton = (Button) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtButton.getImage();
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
			Button swtButton = (Button) SWTUtils.getWidget(viewer, widget);
			firstSwtImage = swtButton.getImage();
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
