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

package org.eclipse.wazaabi.engine.core.tests.nonosgi;

import org.eclipse.wazaabi.engine.edp.adapters.ActionAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.ConditionAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.PropertyChangedEventAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.junit.Assert;
import org.junit.Test;

public class TestEventHandlerAdapter extends TestSequenceAdapter{
	
	private EventHandler eventHandler;
	private EventHandlerAdapter eventHandlerAdapter;
	
	public static final String DISPATCHER_ACTION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.DispatcherActionHandler"; //$NON-NLS-1$ 
	public static final String BAD_DISPATCHER_ACTION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BadDispatcherActionHandler"; //$NON-NLS-1$ 
	private static final String DISPATCHER_CONDITION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.DispatcherConditionHandler";
	private static final String BAD_DISPATCHER_CONDITION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BadDispatcherConditionHandler";

	
	@Override
	public void before() {
		eventHandler = EDPHandlersFactory.eINSTANCE.createEventHandler();
		eventHandlerAdapter = new EventHandlerAdapter();
		eventHandler.eAdapters().add(eventHandlerAdapter);
		super.before();
	}
	
	@Test
	public void testAddActionAdapterToModel() {
		
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(BASIC_ACTION_HANDLER);
		eventHandler.getExecutables().add(action);
		
		Assert.assertTrue(action.eAdapters().get(0) instanceof ActionAdapter);
	}
	
	@Test
	public void testAddConditionAdapterToModel() {
		
		Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		eventHandler.getConditions().add(condition);
		
		Assert.assertTrue(condition.eAdapters().get(0) instanceof ConditionAdapter);

	}
	
	@Test
	public void testAddEventAdapterToModel() {
		
		PropertyChangedEvent propertyChangeEvent = EDPEventsFactory.eINSTANCE
				.createPropertyChangedEvent();
		propertyChangeEvent.setPath("$contextObject/@name");
		eventHandler.getEvents().add(propertyChangeEvent);
		
		Assert.assertTrue(propertyChangeEvent.eAdapters().get(0) instanceof PropertyChangedEventAdapter);
	}
	
	@Test
	public void testTriggerEventHandlerCanExecute() {
		
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(DISPATCHER_ACTION_HANDLER);
		eventHandler.getExecutables().add(action);
		
		Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		condition.setUri(DISPATCHER_CONDITION_HANDLER);
		eventHandler.getConditions().add(condition);
		
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId("core:ui:selection");
		eventHandler.getEvents().add(event);
		
		eventHandlerAdapter.trigger(event);
		
		Assert.assertTrue(((String)event.get("condition")).equalsIgnoreCase("condition executed"));
		Assert.assertTrue(((String)event.get("action")).equalsIgnoreCase("action executed"));
	}
	
	@Test
	public void testTriggerEventHandlerCanNotExecute() {
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(DISPATCHER_ACTION_HANDLER);
		eventHandler.getExecutables().add(action);
		
		Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		condition.setUri(BAD_DISPATCHER_CONDITION_HANDLER);
		eventHandler.getConditions().add(condition);
		
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId("core:ui:selection");
		eventHandler.getEvents().add(event);
		
		Exception exception = null;
		try {
			eventHandlerAdapter.trigger(event);
		} catch (RuntimeException e) {
			exception = e;
			Assert.assertTrue(e instanceof OperationAborted);
		}
		Assert.assertNotNull(exception);		
		Assert.assertTrue(((String)event.get("condition")).equalsIgnoreCase("condition executed"));
		Assert.assertNull(event.get("action"));
	}
	
	@Test
	public void testTriggerEventHandlerWithException() {
		
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(BAD_DISPATCHER_ACTION_HANDLER);
		eventHandler.getExecutables().add(action);
		
		Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		condition.setUri(DISPATCHER_CONDITION_HANDLER);
		eventHandler.getConditions().add(condition);
		
		Event event = EDPEventsFactory.eINSTANCE.createEvent();
		event.setId("core:ui:selection");
		eventHandler.getEvents().add(event);
		
		Exception exception = null;
		try {
			eventHandlerAdapter.trigger(event);
		} catch (RuntimeException e) {
			exception = e;
			Assert.assertTrue(e instanceof OperationAborted);
		}
		Assert.assertNotNull(exception);
		Assert.assertTrue(((String)event.get("condition")).equalsIgnoreCase("condition executed"));
		Assert.assertTrue(((String)event.get("action")).equalsIgnoreCase("action executed"));
	}

}
