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

import junit.framework.Assert;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.coderesolution.reflection.java.codelocators.nonosgi.ReflectionJavaHelper;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.junit.Test;

public class TestStackLayout extends AbstractTestStackLayout {
	
	protected Container container;
	protected PushButton button1;
	protected PushButton button2;
	
	protected Shell swtShell;
	protected Composite swtComposite;
	
	protected Button swtButton;
	protected Button swtButton2;
	
	protected static final String BUTTON1_TEXT = "Hello world";
	protected static final String BUTTON2_TEXT = "This is wazaabi tests";
	
	
	
	@Override
	public void before(){
		super.before();
		// init the 'urn:java' resolver
		ReflectionJavaHelper.init();
		
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
		
		button1 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button1.setText(BUTTON1_TEXT);
		
		button2 = CoreWidgetsFactory.eINSTANCE.createPushButton();
		button2.setText(BUTTON2_TEXT);
		
		
	}
	
	public org.eclipse.swt.widgets.Widget createWazaabiStackLayoutContainingOneButton(boolean before){ //creates a wazaabi container with buttons in it
		if(!before)
			viewer.setContents(container);
		container.getStyleRules().add(stackLayoutRule);
		
		container.getChildren().add(button1);
		
		if(before)
			viewer.setContents(container);
		
		return SWTUtils.getWidget(viewer, container);
		
	}
	
	public void createSWTStackLayoutContainingOneButton() { //creates a wazaabi container with buttons in it
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		swtComposite = new CTabFolder(swtShell, SWT.NONE);
		StackLayout swtStackLayout = new StackLayout();
		swtComposite.setLayout(swtStackLayout);
		
		swtButton = new Button(swtComposite,SWT.PUSH);
		swtButton.setText(BUTTON1_TEXT);
		swtStackLayout.topControl = swtButton;
		
	}
	
	public org.eclipse.swt.widgets.Widget createWazaabiStackLayoutContainingTwoButtons(boolean before){ //creates a wazaabi container with buttons in it
		if(!before)
			viewer.setContents(container);
		container.getStyleRules().add(stackLayoutRule2);
		
		container.getChildren().add(button1);
		
		container.getChildren().add(button2);
		
		if(before)
			viewer.setContents(container);
		
		return SWTUtils.getWidget(viewer, container);
		
	}
	
	public void createSWTStackLayoutContainingTwoButton() { //creates a wazaabi container with buttons in it
		swtShell = new Shell(getDisplay(), SWT.SHELL_TRIM);
		swtShell.setText("SWT");
		swtShell.setLayout(new FillLayout());
		swtShell.setSize(INITIAL_WIDTH, INITIAL_HEIGHT);
		
		swtComposite = new CTabFolder(swtShell, SWT.NONE);
		StackLayout swtStackLayout = new StackLayout();
		swtComposite.setLayout(swtStackLayout);
		
		swtButton = new Button(swtComposite,SWT.PUSH);
		swtButton.setText(BUTTON1_TEXT);
		swtStackLayout.topControl = swtButton;
		
		swtButton2 = new Button(swtComposite,SWT.PUSH);
		swtButton2.setText(BUTTON2_TEXT);
		
	}
	
	public org.eclipse.swt.widgets.Widget createWazaabiStackLayoutContainingTwoButtonsAndRemoveOne(boolean before){
		if(!before)
			viewer.setContents(container);
		container.getStyleRules().add(stackLayoutRule);
		
		container.getChildren().add(button1);
		container.getChildren().add(button2);
		container.getChildren().remove(button2);
		
		
		if(before)
			viewer.setContents(container);
		
		return SWTUtils.getWidget(viewer, container);
		
	}
	
	@Test
	public void testModelAddStackLayoutWithButtonBeforeSetContentsEqualsSWTStackLayout() {
		createSWTStackLayoutContainingOneButton();
		org.eclipse.swt.widgets.Widget composite = createWazaabiStackLayoutContainingOneButton(true);
		Assert.assertTrue(composite instanceof Composite);
		
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)composite).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
		
		org.eclipse.swt.widgets.Control swtControl = ((org.eclipse.swt.custom.StackLayout)swtLayout).topControl;
		Assert.assertTrue(swtControl instanceof Button);
		Assert.assertTrue(BUTTON1_TEXT.equals(((Button)swtControl).getText()));
			
	}
	
	@Test
	public void testModelAddStackLayoutWithButtonAfterSetContentsEqualsSWTStackLayout() {
		createSWTStackLayoutContainingOneButton();
		org.eclipse.swt.widgets.Widget composite = createWazaabiStackLayoutContainingOneButton(false);
		Assert.assertTrue(composite instanceof Composite);
		
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)composite).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
		
		org.eclipse.swt.widgets.Control swtControl = ((org.eclipse.swt.custom.StackLayout)swtLayout).topControl;
		Assert.assertTrue(swtControl instanceof Button);
		Assert.assertTrue(BUTTON1_TEXT.equals(((Button)swtControl).getText()));
		
	}
	
	@Test
	public void testModelAddStackLayoutWithTwoButtonBeforeSetContentsEqualsSWTStackLayout() {
		createSWTStackLayoutContainingTwoButton();
		org.eclipse.swt.widgets.Widget composite = createWazaabiStackLayoutContainingTwoButtons(true);
		Assert.assertTrue(composite instanceof Composite);
		
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)composite).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
		
		org.eclipse.swt.widgets.Control swtControl = ((org.eclipse.swt.custom.StackLayout)swtLayout).topControl;
		Assert.assertTrue(swtControl instanceof Button);
		Assert.assertTrue(BUTTON2_TEXT.equals(((Button)swtControl).getText()));
		
	}
	
	@Test
	public void testModelAddStackLayoutWithTwoButtonAfterSetContentsEqualsSWTStackLayout() {
		createSWTStackLayoutContainingTwoButton();
		org.eclipse.swt.widgets.Widget composite = createWazaabiStackLayoutContainingTwoButtons(false);
		Assert.assertTrue(composite instanceof Composite);
		
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)composite).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
		
		org.eclipse.swt.widgets.Control swtControl = ((org.eclipse.swt.custom.StackLayout)swtLayout).topControl;
		Assert.assertTrue(swtControl instanceof Button);
		Assert.assertTrue(BUTTON2_TEXT.equals(((Button)swtControl).getText()));
		
	}
	
	@Test
	public void testModelAddStackLayoutWithTwoButtonAndRemoveOneBeforeSetContentsEqualsSWTStackLayout(){
		createSWTStackLayoutContainingTwoButton();
		org.eclipse.swt.widgets.Widget composite = createWazaabiStackLayoutContainingTwoButtonsAndRemoveOne(true);
		Assert.assertTrue(composite instanceof Composite);
		
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)composite).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
		
		org.eclipse.swt.widgets.Control swtControl = ((org.eclipse.swt.custom.StackLayout)swtLayout).topControl;
		Assert.assertTrue(swtControl instanceof Button);
		Assert.assertTrue(BUTTON1_TEXT.equals(((Button)swtControl).getText()));
	}
	
	@Test
	public void testModelAddStackLayoutWithTwoButtonAndRemoveOneAfterSetContentsEqualsSWTStackLayout(){
		createSWTStackLayoutContainingTwoButton();
		org.eclipse.swt.widgets.Widget composite = createWazaabiStackLayoutContainingTwoButtonsAndRemoveOne(false);
		Assert.assertTrue(composite instanceof Composite);
		
		org.eclipse.swt.widgets.Layout swtLayout = ((org.eclipse.swt.widgets.Composite)composite).getLayout();
		Assert.assertTrue(swtLayout instanceof org.eclipse.swt.custom.StackLayout);
		
		org.eclipse.swt.widgets.Control swtControl = ((org.eclipse.swt.custom.StackLayout)swtLayout).topControl;
		Assert.assertTrue(swtControl instanceof Button);
		Assert.assertTrue(BUTTON1_TEXT.equals(((Button)swtControl).getText()));
	}


}
