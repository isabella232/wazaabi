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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.ExpandRule;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.TabRule;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.junit.Test;

public class TestLayoutSwitchFromStackLayoutToTabbedLayout extends AbstractTestExpandLayout {
	
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
	protected StackLayoutRule stackLayoutRule;
	
	protected CTabFolder swtTabFolder;
	protected CTabItem swtTab1;
	protected CTabItem swtTab2;
	
	protected Button swtButton;
	protected Button swtButton2;
	
	protected final static String TABLABEL = "tab 1";
	protected final static String TABLABEL2 = "tab 2";
	protected static final String BUTTON1_TEXT = "Hello world";
	protected static final String BUTTON2_TEXT = "This is wazaabi tests";
	protected static final String IMAGE_URN = "urn:java:tabb.png";
	
	
	
	
	@Override
	public void before(){
		super.before();
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
		stackLayoutRule = CoreStylesFactory.eINSTANCE.createStackLayoutRule();
		
		tabbedLayoutRule = CoreStylesFactory.eINSTANCE.createTabbedLayoutRule();
		tabbedLayoutRule.setPropertyName("layout");
		
		tabRule = CoreStylesFactory.eINSTANCE.createTabRule();
		tabRule.setPropertyName("layout-data");
		tabRule.setLabel(TABLABEL);
		tabRule.setImage(IMAGE_URN);
		
		tabRule2 = CoreStylesFactory.eINSTANCE.createTabRule();
		tabRule2.setPropertyName("layout-data");
		tabRule2.setLabel(TABLABEL2);
		
		button1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button1.setText(BUTTON1_TEXT);
		
		button2 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button2.setText(BUTTON2_TEXT);
		
		
	}
	
	public org.eclipse.swt.widgets.Widget createWazaabiStackLayoutWithOneButtonAndChangeStackLayoutRuleToTabbedRule(boolean before){ //creates a wazaabi container with buttons in it
		if(!before)
			viewer.setContents(container);
		container.getStyleRules().add(stackLayoutRule);
		
		container.getChildren().add(button1);
		
		container.getStyleRules().remove(container.getFirstStyleRule("layout", null));
		container.getStyleRules().add(tabbedLayoutRule);
		//button1.getStyleRules().remove(button1.getFirstStyleRule("layout-data", null));
		button1.getStyleRules().add(tabRule);
		
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
		container.getStyleRules().add(stackLayoutRule);
		
		container.getChildren().add(button1);
		
		container.getChildren().add(button2);
		
		container.getStyleRules().remove(container.getFirstStyleRule("layout", null));
		container.getStyleRules().add(tabbedLayoutRule);
		button1.getStyleRules().remove(button1.getFirstStyleRule("layout-data", null));
		button1.getStyleRules().add(tabRule);
		button2.getStyleRules().remove(button2.getFirstStyleRule("layout-data", null));
		button2.getStyleRules().add(tabRule2);
		
		if(before)
			viewer.setContents(container);
		
		getViewer().getUpdateManager().performUpdate();
		
		return SWTUtils.getWidget(viewer, container);
		
	}
	
	
	public void createSWTTabbedLayoutWithOneTabContainingOneButton() throws IOException{ //creates a wazaabi container with buttons in it
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		swtTabFolder = new CTabFolder(swtShell, SWT.NONE);
		
		swtTab1 = new CTabItem(swtTabFolder, SWT.NONE);
		swtTab1.setText(TABLABEL);
		InputStream in = EDPSingletons.getComposedCodeLocator().getResourceInputStream(IMAGE_URN);
		swtTab1.setImage(new Image(display, in));
		
		swtButton = new Button(swtTabFolder,SWT.PUSH);
		swtButton.setText(BUTTON1_TEXT);
		swtTab1.setControl(swtButton);
		
	}
	
	public void createSWTTabbedLayoutWithTwoTabContainingOneButtonEach() throws IOException{ //creates a wazaabi container with buttons in it
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		swtTabFolder = new CTabFolder(swtShell, SWT.NONE);
		
		swtTab1 = new CTabItem(swtTabFolder, SWT.NONE);
		swtTab1.setText(TABLABEL);
		InputStream in = EDPSingletons.getComposedCodeLocator().getResourceInputStream(IMAGE_URN);
		swtTab1.setImage(new Image(display, in));
		
		swtTab2 = new CTabItem(swtTabFolder, SWT.NONE);
		swtTab2.setText(TABLABEL2);
		
		swtButton = new Button(swtTabFolder,SWT.PUSH);
		swtButton.setText(BUTTON1_TEXT);
		swtTab1.setControl(swtButton);
		
		swtButton2 = new Button(swtTabFolder,SWT.PUSH);
		swtButton2.setText(BUTTON2_TEXT);
		swtTab2.setControl(swtButton2);		 
		
	}
	
	
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleBeforeSetContentsEqualsSWTCTabFolder() throws IOException{
		
		createSWTTabbedLayoutWithOneTabContainingOneButton();
		org.eclipse.swt.widgets.Widget tabFolder = createWazaabiStackLayoutWithOneButtonAndChangeStackLayoutRuleToTabbedRule(true);
		Assert.assertTrue(tabFolder instanceof CTabFolder);
		Assert.assertNotNull(((CTabFolder)tabFolder).getItem(0));
		Assert.assertTrue(((CTabFolder)tabFolder).getItem(0) instanceof CTabItem);
		CTabItem tab1 = ((CTabFolder) tabFolder).getItem(0);
		Assert.assertTrue(TABLABEL.equals(tab1.getText()));
		
		Assert.assertTrue(tab1.getControl() instanceof Button);
		Button button = (Button) tab1.getControl();
		Assert.assertEquals(button.getText(),swtButton.getText());
		getViewer().getUpdateManager().performUpdate();
		
		CTabItem wazTab = null;
		for (CTabItem tabs: ((CTabFolder)tabFolder).getItems() ){
			if (tabs.getControl()==button){
				wazTab = tabs;
				break;
			}
		}
		
		Assert.assertNotNull(wazTab);
		Assert.assertEquals(tab1.getBounds(),wazTab.getBounds());
	}
	
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleAfterSetContentsEqualsSWTCTabFolder() throws IOException{
		createSWTTabbedLayoutWithOneTabContainingOneButton();
		org.eclipse.swt.widgets.Widget tabFolder = createWazaabiStackLayoutWithOneButtonAndChangeStackLayoutRuleToTabbedRule(false);
		Assert.assertTrue(tabFolder instanceof CTabFolder);
		Assert.assertNotNull(((CTabFolder)tabFolder).getItem(0));
		Assert.assertTrue(((CTabFolder)tabFolder).getItem(0) instanceof CTabItem);
		CTabItem tab1 = ((CTabFolder) tabFolder).getItem(0);
		Assert.assertTrue(TABLABEL.equals(tab1.getText()));
		
		Assert.assertTrue(tab1.getControl() instanceof Button);
		Button button = (Button) tab1.getControl();
		Assert.assertEquals(button.getText(),swtButton.getText());
		getViewer().getUpdateManager().performUpdate();
		
		CTabItem wazTab = null;
		for (CTabItem tabs: ((CTabFolder)tabFolder).getItems() ){
			if (tabs.getControl()==button){
				wazTab = tabs;
				break;
			}
		}
		
		Assert.assertNotNull(wazTab);
		Assert.assertEquals(tab1.getBounds(),wazTab.getBounds());
	}
	
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleWithTwoTabsBeforeSetContentsEqualsSWTCTabFolder() throws IOException{
		createSWTTabbedLayoutWithTwoTabContainingOneButtonEach();
		org.eclipse.swt.widgets.Widget tabFolder = createWazaabiExpandLayoutWithTwoExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(true);
		Assert.assertTrue(tabFolder instanceof CTabFolder);
		Assert.assertEquals(2,((CTabFolder)tabFolder).getItemCount());
		Assert.assertNotNull(((CTabFolder)tabFolder).getItem(0));
		Assert.assertTrue(((CTabFolder)tabFolder).getItem(0) instanceof CTabItem);
		CTabItem tab1 = ((CTabFolder) tabFolder).getItem(0);
		Assert.assertTrue(tab1.getText().equals(TABLABEL));
		
		Assert.assertTrue(tab1.getControl() instanceof Button);		
		Button button = (Button) tab1.getControl();
		Assert.assertEquals(button.getText(),swtButton.getText());
		
		CTabItem wazTab = null;
		for (CTabItem tabs: ((CTabFolder)tabFolder).getItems() ){
			if (tabs.getControl()==button){
				wazTab = tabs;
				break;
			}
		}
		Assert.assertNotNull(wazTab);
		Assert.assertEquals(tab1.getBounds(),wazTab.getBounds());
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button1, swtButton));
		
		Assert.assertNotNull(((CTabFolder)tabFolder).getItem(1));
		Assert.assertTrue(((CTabFolder)tabFolder).getItem(1) instanceof CTabItem);
		CTabItem tab2 = ((CTabFolder) tabFolder).getItem(1);
		Assert.assertTrue(tab2.getText().equals(TABLABEL2));
		
		Assert.assertTrue(tab2.getControl() instanceof Button);		
		Button buttonbis = (Button) tab2.getControl();
		Assert.assertEquals(buttonbis.getText(),swtButton2.getText());
		
		CTabItem wazTab2 = null;
		for (CTabItem tabs: ((CTabFolder)tabFolder).getItems() ){
			if (tabs.getControl()==buttonbis){
				wazTab2 = tabs;
				break;
			}
		}
		Assert.assertNotNull(wazTab2);
		Assert.assertEquals(tab2.getBounds(),wazTab2.getBounds());
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button2, swtButton2));
	}
	@Test
	public void testSwitchFromExpandLayoutRuleToTabbedLayoutRuleWithTwoTabsAfterSetContentsEqualsSWTCTabFolder() throws IOException{
		createSWTTabbedLayoutWithTwoTabContainingOneButtonEach();
		org.eclipse.swt.widgets.Widget tabFolder = createWazaabiExpandLayoutWithTwoExpandContainingOneButtonAndChangeExpandRuleToTabbedRule(false);
		Assert.assertTrue(tabFolder instanceof CTabFolder);
		Assert.assertEquals(2,((CTabFolder)tabFolder).getItemCount());
		Assert.assertNotNull(((CTabFolder)tabFolder).getItem(0));
		Assert.assertTrue(((CTabFolder)tabFolder).getItem(0) instanceof CTabItem);
		CTabItem tab1 = ((CTabFolder) tabFolder).getItem(0);
		Assert.assertTrue(tab1.getText().equals(TABLABEL));
		
		Assert.assertTrue(tab1.getControl() instanceof Button);		
		Button button = (Button) tab1.getControl();
		Assert.assertEquals(button.getText(),swtButton.getText());
		
		CTabItem wazTab = null;
		for (CTabItem tabs: ((CTabFolder)tabFolder).getItems() ){
			if (tabs.getControl()==button){
				wazTab = tabs;
				break;
			}
		}
		Assert.assertNotNull(wazTab);
		Assert.assertEquals(tab1.getBounds(),wazTab.getBounds());
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button1, swtButton));
		
		Assert.assertNotNull(((CTabFolder)tabFolder).getItem(1));
		Assert.assertTrue(((CTabFolder)tabFolder).getItem(1) instanceof CTabItem);
		CTabItem tab2 = ((CTabFolder) tabFolder).getItem(1);
		Assert.assertTrue(tab2.getText().equals(TABLABEL2));
		
		Assert.assertTrue(tab2.getControl() instanceof Button);		
		Button buttonbis = (Button) tab2.getControl();
		Assert.assertEquals(buttonbis.getText(),swtButton2.getText());
		
		CTabItem wazTab2 = null;
		for (CTabItem tabs: ((CTabFolder)tabFolder).getItems() ){
			if (tabs.getControl()==buttonbis){
				wazTab2 = tabs;
				break;
			}
		}
		Assert.assertNotNull(wazTab2);
		Assert.assertEquals(tab2.getBounds(),wazTab2.getBounds());
		//Assert.assertTrue(SWTUtils.haveSameBounds(viewer, button2, swtButton2));
	}
}
