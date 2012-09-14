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

import java.io.IOException;
import java.io.InputStream;

import junit.framework.Assert;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.ExpandRule;
import org.eclipse.wazaabi.mm.core.styles.TabRule;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.junit.Test;

public class TestLayoutSwitchFromTabbedToExpandLayout extends AbstractTestExpandLayout {
	
	protected Container container;
	protected ExpandLayoutRule expandLayoutRule;
	protected ExpandRule expandRule;
	protected ExpandRule expandRule2;
	protected PushButton button1;
	protected PushButton button2;
	
	protected TabbedLayoutRule tabbedLayoutRule;
	protected TabRule tabRule;
	protected TabRule tabRule2;
	
	protected Shell swtShell;
	protected ExpandBar swtExpandBar;
	protected ExpandItem swtExpand1;
	protected ExpandItem swtExpand2;
	
	protected CTabFolder swtTabFolder;
	protected CTabItem swtTab1;
	protected CTabItem swtTab2;
	
	protected Button swtButton;
	protected Button swtButton2;
	
	protected final static String EXPANDLABEL = "tab 1";
	protected final static String EXPANDLABEL2 = "tab 2";
	protected static final String BUTTON1_TEXT = "Hello world";
	protected static final String BUTTON2_TEXT = "This is wazaabi tests";
	protected static final boolean EXPANDED1 = true;
	protected static final boolean EXPANDED2 = false;
	protected static final String IMAGE_URN = "urn:java:tabb.png";
	
	
	
	
	@Override
	public void before(){
		super.before();
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
		expandLayoutRule = CoreStylesFactory.eINSTANCE.createExpandLayoutRule();
		expandLayoutRule.setPropertyName("layout");
		
		tabbedLayoutRule = CoreStylesFactory.eINSTANCE.createTabbedLayoutRule();
		tabbedLayoutRule.setPropertyName("layout");
		
		expandRule = CoreStylesFactory.eINSTANCE.createExpandRule();
		expandRule.setPropertyName("layout-data");
		expandRule.setLabel(EXPANDLABEL);
		expandRule.setExpanded(EXPANDED1);
		expandRule.setImage(IMAGE_URN);
		
		expandRule2 = CoreStylesFactory.eINSTANCE.createExpandRule();
		expandRule2.setPropertyName("layout-data");
		expandRule2.setLabel(EXPANDLABEL2);
		expandRule2.setExpanded(EXPANDED2);
		
		tabRule = CoreStylesFactory.eINSTANCE.createTabRule();
		tabRule.setPropertyName("layout-data");
		tabRule.setLabel(EXPANDLABEL);
		tabRule.setImage(IMAGE_URN);
		
		tabRule2 = CoreStylesFactory.eINSTANCE.createTabRule();
		tabRule2.setPropertyName("layout-data");
		tabRule2.setLabel(EXPANDLABEL2);
		
		button1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button1.setText(BUTTON1_TEXT);
		
		button2 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button2.setText(BUTTON2_TEXT);
		
		
	}
	
	public org.eclipse.swt.widgets.Widget createWazaabiExpandLayoutWithOneExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(boolean before){ //creates a wazaabi container with buttons in it
		if(!before)
			viewer.setContents(container);
		container.getStyleRules().add(tabbedLayoutRule);
		
		button1.getStyleRules().add(tabRule);
		container.getChildren().add(button1);
		
		container.getStyleRules().remove(container.getFirstStyleRule("layout", null));
		container.getStyleRules().add(expandLayoutRule);
		button1.getStyleRules().remove(button1.getFirstStyleRule("layout-data", null));
		button1.getStyleRules().add(expandRule);
		
		if(before)
			viewer.setContents(container);
		
		getViewer().getUpdateManager().performUpdate();
		
//		mainShell.open();
//		while (!swtShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
		
		return SWTUtils.getWidget(viewer, container);
		
	}
	
	public org.eclipse.swt.widgets.Widget createWazaabiExpandLayoutWithTwoExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(boolean before){
		if(!before)
			viewer.setContents(container);
		container.getStyleRules().add(tabbedLayoutRule);
		
		button1.getStyleRules().add(tabRule);
		container.getChildren().add(button1);
		
		button2.getStyleRules().add(tabRule2);
		container.getChildren().add(button2);
		
		container.getStyleRules().remove(container.getFirstStyleRule("layout", null));
		container.getStyleRules().add(expandLayoutRule);
		button1.getStyleRules().remove(button1.getFirstStyleRule("layout-data", null));
		button1.getStyleRules().add(expandRule);
		button2.getStyleRules().remove(button2.getFirstStyleRule("layout-data", null));
		button2.getStyleRules().add(expandRule2);
		
		if(before)
			viewer.setContents(container);
		
		getViewer().getUpdateManager().performUpdate();
		
		return SWTUtils.getWidget(viewer, container);
		
	}
	
	
	public void createSWTExpandBarWithOneExpandContainingOneButton() throws IOException{ //creates a wazaabi container with buttons in it
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		swtExpandBar = new ExpandBar(swtShell, SWT.NONE);
		
		swtExpand1 = new ExpandItem(swtExpandBar, SWT.NONE);
		swtExpand1.setText(EXPANDLABEL);
		swtExpand1.setExpanded(EXPANDED1);
		InputStream in = EDPSingletons.getComposedCodeLocator().getResourceInputStream(IMAGE_URN);
		swtExpand1.setImage(new Image(display, in));
		
		swtButton = new Button(swtExpandBar,SWT.PUSH);
		swtButton.setText(BUTTON1_TEXT);
		swtExpand1.setControl(swtButton);
		//TODO This ugly size setting is only here to reflect the actual implementation of Wazaabi and should be corrected whenever SWTContainerView is updated! 
		swtExpand1.setHeight(swtButton.computeSize(SWT.DEFAULT, SWT.DEFAULT).y+100);
		
		getViewer().getUpdateManager().performUpdate();
		
//		swtShell.open();
//		while (!swtShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
		
	}
	
	public void createSWTExpandLayoutWithTwoExpandContainingOneButtonEach() throws IOException{ //creates a wazaabi container with buttons in it
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		swtExpandBar = new ExpandBar(swtShell, SWT.NONE);
		
		swtExpand1 = new ExpandItem(swtExpandBar, SWT.NONE);
		swtExpand1.setText(EXPANDLABEL);
		swtExpand1.setExpanded(EXPANDED1);
		InputStream in = EDPSingletons.getComposedCodeLocator().getResourceInputStream(IMAGE_URN);
		swtExpand1.setImage(new Image(display, in));

		swtExpand2 = new ExpandItem(swtExpandBar, SWT.NONE);
		swtExpand2.setText(EXPANDLABEL2);
		swtExpand2.setExpanded(EXPANDED2);

		swtButton = new Button(swtExpandBar, SWT.PUSH);
		swtButton.setText(BUTTON1_TEXT);
		swtExpand1.setControl(swtButton);
		// TODO This ugly size setting is only here to reflect the actual
		// implementation of Wazaabi and should be corrected whenever
		// SWTContainerView is updated!
		swtExpand1
				.setHeight(swtButton.computeSize(SWT.DEFAULT, SWT.DEFAULT).y + 100);

		swtButton2 = new Button(swtExpandBar, SWT.PUSH);
		swtButton2.setText(BUTTON2_TEXT);
		swtExpand2.setControl(swtButton2);
		// TODO This ugly size setting is only here to reflect the actual
		// implementation of Wazaabi and should be corrected whenever
		// SWTContainerView is updated!
		swtExpand2
				.setHeight(swtButton.computeSize(SWT.DEFAULT, SWT.DEFAULT).y + 100);
		
		getViewer().getUpdateManager().performUpdate();
//		swtShell.open();
//		while (!swtShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}

	}
	
	
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleBeforeSetContentsEqualsSWTCTabFolder() throws IOException{
		
		createSWTExpandBarWithOneExpandContainingOneButton();
		org.eclipse.swt.widgets.Widget expandBar = createWazaabiExpandLayoutWithOneExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(true);
		Assert.assertTrue(expandBar instanceof ExpandBar);
		Assert.assertNotNull(((ExpandBar)expandBar).getItem(0));
		Assert.assertTrue(((ExpandBar)expandBar).getItem(0) instanceof ExpandItem);
		ExpandItem expand1 = ((ExpandBar) expandBar).getItem(0);
		Assert.assertEquals(EXPANDLABEL, expand1.getText());
		Assert.assertEquals(EXPANDED1, expand1.getExpanded());
		
		Assert.assertTrue(expand1.getControl() instanceof Button);
		Button button = (Button) expand1.getControl();
		Assert.assertEquals(swtButton.getText(),button.getText());
		getViewer().getUpdateManager().performUpdate();
		
		ExpandItem wazExpand = null;
		for (ExpandItem expands: ((ExpandBar)expandBar).getItems() ){
			if (expands.getControl()==button){
				wazExpand = expands;
				break;
			}
		}
		Assert.assertNotNull(wazExpand);
		Assert.assertEquals(expand1.getHeight(),wazExpand.getHeight());
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button1, swtButton));
	}
	
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleAfterSetContentsEqualsSWTCTabFolder() throws IOException{
		createSWTExpandBarWithOneExpandContainingOneButton();
		org.eclipse.swt.widgets.Widget expandBar = createWazaabiExpandLayoutWithOneExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(false);
		Assert.assertTrue(expandBar instanceof ExpandBar);
		Assert.assertNotNull(((ExpandBar)expandBar).getItem(0));
		Assert.assertTrue(((ExpandBar)expandBar).getItem(0) instanceof ExpandItem);
		ExpandItem expand1 = ((ExpandBar) expandBar).getItem(0);
		Assert.assertEquals(EXPANDLABEL, expand1.getText());
		Assert.assertEquals(EXPANDED1, expand1.getExpanded());
		
		Assert.assertTrue(expand1.getControl() instanceof Button);
		Button button = (Button) expand1.getControl();
		Assert.assertEquals(swtButton.getText(),button.getText());
		getViewer().getUpdateManager().performUpdate();
		
		ExpandItem wazExpand = null;
		for (ExpandItem expands: ((ExpandBar)expandBar).getItems() ){
			if (expands.getControl()==button){
				wazExpand = expands;
				break;
			}
		}
		Assert.assertNotNull(wazExpand);
		Assert.assertEquals(expand1.getHeight(),wazExpand.getHeight());
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button1, swtButton));
	}
	
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleWithTwoTabsBeforeSetContentsEqualsSWTCTabFolder() throws IOException{
		createSWTExpandLayoutWithTwoExpandContainingOneButtonEach();
		org.eclipse.swt.widgets.Widget expandBar = createWazaabiExpandLayoutWithTwoExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(true);
		Assert.assertTrue(expandBar instanceof ExpandBar);
		Assert.assertEquals(2,((ExpandBar)expandBar).getItemCount());
		
		Assert.assertNotNull(((ExpandBar)expandBar).getItem(0));
		Assert.assertTrue(((ExpandBar)expandBar).getItem(0) instanceof ExpandItem);
		ExpandItem expand1 = ((ExpandBar) expandBar).getItem(0);
		Assert.assertEquals(EXPANDLABEL, expand1.getText());
		Assert.assertEquals(EXPANDED1, expand1.getExpanded());
		
		Assert.assertTrue(expand1.getControl() instanceof Button);		
		Button button = (Button) expand1.getControl();
		Assert.assertEquals(swtButton.getText(),button.getText());
		
		ExpandItem wazExpand = null;
		for (ExpandItem expands: ((ExpandBar)expandBar).getItems() ){
			if (expands.getControl()==button){
				wazExpand = expands;
				break;
			}
		}
		Assert.assertNotNull(wazExpand);
		Assert.assertEquals(expand1.getHeight(),wazExpand.getHeight());
		
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button1, swtButton));
		
		Assert.assertNotNull(((ExpandBar)expandBar).getItem(1));
		Assert.assertTrue(((ExpandBar)expandBar).getItem(1) instanceof ExpandItem);
		ExpandItem expand2 = ((ExpandBar) expandBar).getItem(1);
		Assert.assertEquals(EXPANDLABEL2, expand2.getText());
		Assert.assertEquals(EXPANDED2, expand2.getExpanded());
		
		Assert.assertTrue(expand2.getControl() instanceof Button);		
		Button buttonbis = (Button) expand2.getControl();
		Assert.assertEquals(swtButton2.getText(),buttonbis.getText());
		
		ExpandItem wazExpand2 = null;
		for (ExpandItem expands: ((ExpandBar)expandBar).getItems() ){
			if (expands.getControl()==buttonbis){
				wazExpand2 = expands;
				break;
			}
		}
		Assert.assertNotNull(wazExpand2);
		Assert.assertEquals(expand2.getHeight(),wazExpand.getHeight());
		
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button2, swtButton2));
	}
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleWithTwoTabsAfterSetContentsEqualsSWTCTabFolder() throws IOException{
		createSWTExpandLayoutWithTwoExpandContainingOneButtonEach();
		org.eclipse.swt.widgets.Widget expandBar = createWazaabiExpandLayoutWithTwoExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(false);
		Assert.assertTrue(expandBar instanceof ExpandBar);
		Assert.assertEquals(2,((ExpandBar)expandBar).getItemCount());
		
		Assert.assertNotNull(((ExpandBar)expandBar).getItem(0));
		Assert.assertTrue(((ExpandBar)expandBar).getItem(0) instanceof ExpandItem);
		ExpandItem expand1 = ((ExpandBar) expandBar).getItem(0);
		Assert.assertEquals(EXPANDLABEL, expand1.getText());
		Assert.assertEquals(EXPANDED1, expand1.getExpanded());
		
		Assert.assertTrue(expand1.getControl() instanceof Button);		
		Button button = (Button) expand1.getControl();
		Assert.assertEquals(swtButton.getText(),button.getText());
		
		ExpandItem wazExpand = null;
		for (ExpandItem expands: ((ExpandBar)expandBar).getItems() ){
			if (expands.getControl()==button){
				wazExpand = expands;
				break;
			}
		}
		Assert.assertNotNull(wazExpand);
		Assert.assertEquals(expand1.getHeight(),wazExpand.getHeight());
		
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button1, swtButton));
		
		Assert.assertNotNull(((ExpandBar)expandBar).getItem(1));
		Assert.assertTrue(((ExpandBar)expandBar).getItem(1) instanceof ExpandItem);
		ExpandItem expand2 = ((ExpandBar) expandBar).getItem(1);
		Assert.assertEquals(EXPANDLABEL2, expand2.getText());
		Assert.assertEquals(EXPANDED2, expand2.getExpanded());
		
		Assert.assertTrue(expand2.getControl() instanceof Button);		
		Button buttonbis = (Button) expand2.getControl();
		Assert.assertEquals(swtButton2.getText(),buttonbis.getText());
		
		ExpandItem wazExpand2 = null;
		for (ExpandItem expands: ((ExpandBar)expandBar).getItems() ){
			if (expands.getControl()==buttonbis){
				wazExpand2 = expands;
				break;
			}
		}
		Assert.assertNotNull(wazExpand2);
		Assert.assertEquals(expand2.getHeight(),wazExpand.getHeight());
		
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button2, swtButton2));
	}
}
