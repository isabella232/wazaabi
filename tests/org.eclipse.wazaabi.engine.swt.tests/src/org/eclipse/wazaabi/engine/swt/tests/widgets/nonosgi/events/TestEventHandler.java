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

package org.eclipse.wazaabi.engine.swt.tests.widgets.nonosgi.events;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.swt.SWT;
import org.eclipse.wazaabi.engine.edp.adapters.ActionAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.tests.ReflectionUtils;
import org.eclipse.wazaabi.engine.swt.tests.AbstractCommandTest;
import org.eclipse.wazaabi.engine.swt.tests.SWTUtils;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
import org.junit.Assert;
import org.junit.Test;

public class TestEventHandler extends AbstractCommandTest{

	public static final String JAVA_URN_TEST_URI = "urn:java:org.eclipse.wazaabi.engine.swt.tests.widgets.nonosgi.events.handlers.ButtonHandler"; //$NON-NLS-1$ 
	public static final String BASIC_ACTION = "urn:java:org.eclipse.wazaabi.engine.swt.tests.widgets.nonosgi.events.handlers.BasicActionHandler"; //$NON-NLS-1$
	public static final String BASIC_CONDITION = "urn:java:org.eclipse.wazaabi.engine.swt.tests.widgets.nonosgi.events.handlers.BasicConditionHandler"; //$NON-NLS-1$
	private Label label;
	private TextComponent textComponent;
	private Container container;
	

	/**
	 * Iterates on EventHandlerAdapters attached to this EventHandler
	 * and returns the result of the call of 'getCounter'.
	 * 
	 * @param eventHandler
	 * @return the result of the call, -1 otherwise
	 */
	protected int getCounter(ActionAdapter adapter) {
		if (adapter == null)
			return -1;
		return (Integer) ReflectionUtils.invokeMethod(
				adapter.getCodeDescriptor(), "getCounter", null, int.class,
				null);
//				
//				EventHandlerUtils.getUniqueMethodDescriptor(
//				eventHandler, "getCounter", null, int.class).invokeMethod(null);
	}
	
	@Override
	public void before() {
		super.before();
		label = CoreWidgetsFactory.eINSTANCE.createLabel();
		textComponent = CoreWidgetsFactory.eINSTANCE.createTextComponent();
		TextComponent textComponent2 = CoreWidgetsFactory.eINSTANCE.createTextComponent();
		container = CoreWidgetsFactory.eINSTANCE.createContainer();
		
		StringRule textRule = CoreStylesFactory.eINSTANCE.createStringRule();
		textRule.setPropertyName("text"); //$NON-NLS-1$
		textRule.setValue("Label text");
		label.getStyleRules().add(textRule);
		
		GridLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createGridLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);
		
		container.getChildren().add(label);
		container.getChildren().add(textComponent);
		container.getChildren().add(textComponent2);

	}
	
	@Override
	public void after() {
		
//		while (!mainShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
		
		super.after();
	}

	@Test
	public void TestEventHandlerWithErrorHandling() {

		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		
		textComponent.getHandlers().add(eventHandler);
		textComponent.setText("start");
		
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId("core:ui:focus:out");
		eventHandler.getEvents().add(event);
		
		Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		condition.setUri(BASIC_CONDITION);
		eventHandler.getConditions().add(condition);
		
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(BASIC_ACTION);
		eventHandler.getExecutables().add(action);
		

		// render the content
		viewer.setContents(container);
		
		Adapter adapter = eventHandler.eAdapters().get(0);
		Assert.assertNotNull(adapter);
		Assert.assertTrue(adapter instanceof EventHandlerAdapter);
		((EventHandlerAdapter) adapter).trigger(event);
		
		Assert.assertTrue(((String)textComponent.get("condition")).equalsIgnoreCase("condition executed"));
		Assert.assertTrue(((String)textComponent.get("action")).equalsIgnoreCase("action executed"));
		
		Exception exception = null;
		try {
			((EventHandlerAdapter) adapter).trigger(event);
		} catch (RuntimeException e) {
			exception = e;
		}
		Assert.assertNotNull(exception);
		Assert.assertTrue(exception instanceof OperationAborted);
		
		//Assert.assertTrue(textComponent.getErrorText() != null);
		mainShell.open();

	}

	@Test
	public void testEventHandlerAddedBeforeSetContent() {
		
		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();

		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		pushButton.getHandlers().add(eventHandler);

		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(JAVA_URN_TEST_URI);
		eventHandler.getExecutables().add(action);

		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		eventHandler.getEvents().add(event);
		event.setId("core:ui:selection");

		// Set the content
		viewer.setContents(pushButton);
		
		Adapter adapter = (ActionAdapter) action.eAdapters().get(0);
		Assert.assertNotNull(adapter);
		Assert.assertTrue(adapter instanceof ActionAdapter);
		Assert.assertEquals(0, getCounter((ActionAdapter) adapter));
		org.eclipse.swt.widgets.Button swtButton = (org.eclipse.swt.widgets.Button) SWTUtils
				.getWidget(viewer, pushButton);

		mainShell.open();

		// send 10 fake 'selection' events to the SWT widget
		for (int i = 0; i < 10; i++)
			swtButton.notifyListeners(SWT.Selection, null);

		while (display.readAndDispatch());

		Assert.assertEquals(10, getCounter((ActionAdapter) adapter));

	}

	@Test
	public void testEventHandlerAddedAfterSetContent() {
		
		PushButton pushButton = CoreWidgetsFactory.eINSTANCE.createPushButton();
		// Set the content
		viewer.setContents(pushButton);
		
		EventHandler eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		pushButton.getHandlers().add(eventHandler);

		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(JAVA_URN_TEST_URI);
		eventHandler.getExecutables().add(action);

		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		eventHandler.getEvents().add(event);
		event.setId("core:ui:selection");

		Adapter adapter = (ActionAdapter) action.eAdapters().get(0);
		Assert.assertNotNull(adapter);
		Assert.assertTrue(adapter instanceof ActionAdapter);
		Assert.assertEquals(0, getCounter((ActionAdapter) adapter));
		org.eclipse.swt.widgets.Button swtButton = (org.eclipse.swt.widgets.Button) SWTUtils
				.getWidget(viewer, pushButton);

		mainShell.open();

		// send 10 fake 'selection' events to the SWT widget
		for (int i = 0; i < 10; i++)
			swtButton.notifyListeners(SWT.Selection, null);

		while (display.readAndDispatch());

		Assert.assertEquals(10, getCounter((ActionAdapter) adapter));

	}

}
