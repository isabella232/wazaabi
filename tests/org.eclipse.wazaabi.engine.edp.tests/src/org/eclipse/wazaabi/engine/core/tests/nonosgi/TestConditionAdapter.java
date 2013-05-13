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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import junit.framework.Assert;

import org.eclipse.wazaabi.engine.edp.adapters.ConditionAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.tests.ReflectionUtils;
import org.eclipse.wazaabi.locator.platform.plugin.codedescriptors.PluginCodeDescriptor;
import org.eclipse.wazaabi.locator.urn.java.codedescriptors.JavaCodeDescriptor;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.junit.Test;

public class TestConditionAdapter extends AbstractTestOperationAdapter {

	public static final String BASIC_CONDITION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BasicConditionHandler"; //$NON-NLS-1$ 
	public static final String BAD_CONDITION_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BadConditionHandler"; //$NON-NLS-1$ 

	@Test
	public void testAddAdapterToModel() {
		Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		condition.setUri(BASIC_CONDITION_HANDLER);
		ConditionAdapter conditionAdapter = new ConditionAdapter();
		condition.eAdapters().add(conditionAdapter);
		AbstractCodeDescriptor codeDescriptor = conditionAdapter.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_CONDITION_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				conditionAdapter.getCodeDescriptor(), "canExecute", new Class[] {
						int.class, int.class }, boolean.class,
				new Object[] { 2, 2 });
		assertTrue(result instanceof Boolean);
		assertTrue(((Boolean) result).booleanValue() == true);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
		condition.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
	}

	
	@Test
	public void testAddAdapterToModelWithException() {
		Condition condition = EDPHandlersFactory.eINSTANCE.createCondition();
		condition.setUri(BAD_CONDITION_HANDLER);
		ConditionAdapter conditionAdapter = new ConditionAdapter();
		condition.eAdapters().add(conditionAdapter);
		AbstractCodeDescriptor codeDescriptor = conditionAdapter.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BAD_CONDITION_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = null;
		try {
			result = ReflectionUtils.invokeMethod(
					conditionAdapter.getCodeDescriptor(), "canExecute", new Class[] {
							int.class, int.class }, boolean.class,
					new Object[] { 2, 2 });
		} catch (RuntimeException e) {
			Assert.assertTrue(e.getCause() instanceof OperationAborted);
		}
		Assert.assertNull(result);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
		condition.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
	}
	
	@Test
	public void testIsAdapterForTypeObject() {
		ConditionAdapter conditionAdapter = new ConditionAdapter();
		assertTrue(conditionAdapter.isAdapterForType(EDPHandlersFactory.eINSTANCE.createCondition()));
	}


	@Test
	public void testRemoveAdapterFromModel() {
		Condition model = EDPHandlersFactory.eINSTANCE.createCondition();
		model.setUri(BASIC_CONDITION_HANDLER);
		ConditionAdapter conditionAdapter = new ConditionAdapter();
		model.eAdapters().add(conditionAdapter);
		AbstractCodeDescriptor codeDescriptor = conditionAdapter.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_CONDITION_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				conditionAdapter.getCodeDescriptor(), "canExecute", new Class[] {
						int.class, int.class }, boolean.class,
				new Object[] { 2, 4 });
		assertTrue(result instanceof Boolean);
		assertTrue(((Boolean) result).booleanValue() == false);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
		model.eAdapters().remove(conditionAdapter);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));

	}
	
}
