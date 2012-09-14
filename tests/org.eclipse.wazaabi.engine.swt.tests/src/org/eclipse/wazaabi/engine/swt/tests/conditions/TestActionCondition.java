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

package org.eclipse.wazaabi.engine.swt.tests.conditions;

import junit.framework.Assert;

import org.eclipse.wazaabi.engine.edp.adapters.ActionAdapter;
import org.eclipse.wazaabi.engine.edp.adapters.ConditionAdapter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.junit.Test;

public class TestActionCondition extends AbstractTestCondition{

	private Action action;
	private EventHandler eventHandler;
	private String actionURI = "urn:java:org.eclipse.wazaabi.engine.swt.tests.conditions.VerySimpleAction";
	private Widget widget;
	
	private String text = "newText";
	
	
	@Override 
	public void before(){
		action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(actionURI);
		eventHandler.getExecutables().add(action);
		widget = CoreWidgetsFactory.eINSTANCE.createLabel();
		super.before();
	}
	
	public void after(){
		super.after();
	}
	
	@Test
	public void testModelSetConditionBeforeViewerSetContentsCanExecute(){
		widget = ModelSetActionAndCondition(true, widget, eventHandler, condition);

		Assert.assertTrue(((ConditionAdapter)(action.eAdapters().get(0))).canExecute(null, null, null));
//		AbstractCodeDescriptor codeDescriptor = ((ActionAdapter)(action.eAdapters().get(0))).getCodeDescriptor();
	
//		Assert.assertTrue(((ConditionAdapter)(condition.eAdapters().get(0))).check(null, null, null));
//		Assert.assertEquals(text,((Label)(widget)).getText());
	}
	@Test
	public void testModelSetMultipleConditionsBeforeViewerSetContentsCheck(){
		widget = ModelSetActionAndMultipleConditions(true, widget, eventHandler, condition, condition2);
		Assert.assertFalse(((ConditionAdapter)(action.eAdapters().get(0))).canExecute(null, null, null));
	}
	@Test
	public void testModelRemoveConditionBeforeViewerSetContentsCanExecute(){
		widget = ModelSetActionAndConditionAndRemoveCondition(true, widget, eventHandler, condition);
		Assert.assertTrue(((ConditionAdapter)(action.eAdapters().get(0))).canExecute(null, null, null));
	}
	
	@Test
	public void testModelSetConditionBeforeViewerSetContentsActionTrigger(){
		widget = ModelSetActionAndCondition(true, widget, eventHandler, condition);
		try {
			((ActionAdapter)(action.eAdapters().get(0))).trigger(null, null, null);
		} catch (OperationAborted e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Assert.assertEquals(text, ((Label)(widget)).getText()); 
	}
	
	@Test
	public void testModelSetMultipleConditionsBeforeViewerSetContentsActionTrigger(){
		widget = ModelSetActionAndMultipleConditions(true, widget, eventHandler, condition, condition2);
		try {
			((ActionAdapter)(action.eAdapters().get(0))).trigger(null, null, null);
		} catch (OperationAborted e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Assert.assertNull(((Label)(widget)).getText());
	}
	
	@Test 
	public void testModelRemoveConditionBeforeViewerSetContentsActionTrigger(){
		widget = ModelSetActionAndConditionAndRemoveCondition(true, widget, eventHandler, condition);
		try {
			((ActionAdapter)(action.eAdapters().get(0))).trigger(null, null, null);
		} catch (OperationAborted e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Assert.assertEquals(text, ((Label)(widget)).getText());
	}
	
	
}
