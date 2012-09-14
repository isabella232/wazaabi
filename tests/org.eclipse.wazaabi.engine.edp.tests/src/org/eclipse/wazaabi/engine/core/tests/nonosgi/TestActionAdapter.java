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

import org.eclipse.wazaabi.coderesolution.reflection.java.codedescriptors.JavaCodeDescriptor;
import org.eclipse.wazaabi.coderesolution.reflection.java.plugins.codedescriptors.PluginCodeDescriptor;
import org.eclipse.wazaabi.engine.core.tests.osgi.TestCodeDescriptorResolutionInOSGIMode;
import org.eclipse.wazaabi.engine.edp.adapters.ActionAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.tests.OsgiUtils;
import org.eclipse.wazaabi.engine.edp.tests.ReflectionUtils;
import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.junit.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class TestActionAdapter extends AbstractTestOperationAdapter {

	@Test
	public void testAddAdapterToModel() {
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(BASIC_ACTION_HANDLER);
		ActionAdapter actionAdapter = new ActionAdapter();
		action.eAdapters().add(actionAdapter);
		AbstractCodeDescriptor codeDescriptor = actionAdapter.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_ACTION_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				codeDescriptor, "execute", new Class[] {
						int.class, int.class }, int.class,
				new Object[] { 2, 4 });
		assertTrue(result instanceof Integer);
		assertTrue(((Integer) result).intValue() == 2 - 4);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
		action.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
	}
	
	@Test
	public void testAddAdapterToModelWithException() {
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		action.setUri(BAD_ACTION_HANDLER);
		ActionAdapter actionAdapter = new ActionAdapter();
		action.eAdapters().add(actionAdapter);
		AbstractCodeDescriptor codeDescriptor = actionAdapter.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BAD_ACTION_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = null;
		try {
			result = ReflectionUtils.invokeMethod(
					codeDescriptor, "execute", new Class[] {
							int.class, int.class }, int.class,
					new Object[] { 2, 4 });
		} catch (RuntimeException e) {
			Assert.assertTrue(e.getCause() instanceof OperationAborted);
		}
		Assert.assertNull(result);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
		action.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
	}

	@Test
	public void testIsAdapterForTypeObject() {
		ActionAdapter actionAdapter = new ActionAdapter();
		assertTrue(actionAdapter.isAdapterForType(EDPHandlersFactory.eINSTANCE.createAction()));
	}

	public static final String PLUGIN_PLATFORM_TEST_URI = "platform:/plugin/org.eclipse.wazaabi.engine.edp.tests/org.eclipse.wazaabi.engine.edp.tests.handlers.BasicActionHandler"; //$NON-NLS-1$ 
	
	@Test
	public void testNotifyChangedNotification() {
		BundleContext context = OsgiUtils.launchOsgiFwk();
		Bundle bundle = OsgiUtils.installBundle(context, "",
				TestCodeDescriptorResolutionInOSGIMode.TEST_PLUGIN_FILE_NAME, getClass()
						.getClassLoader());
		assertTrue(context != null);
		Action action = EDPHandlersFactory.eINSTANCE.createAction();
		// step 1 : we set the uri to FirstTestHandler
		action.setUri(BASIC_ACTION_HANDLER);
		ActionAdapter actionAdapter = new ActionAdapter();
		action.eAdapters().add(actionAdapter);
		AbstractCodeDescriptor firstHandlerCodeDescriptor = actionAdapter
				.getCodeDescriptor();
		// we verify the uri
		assertTrue(firstHandlerCodeDescriptor != null);
		assertTrue(BASIC_ACTION_HANDLER
				.equals(firstHandlerCodeDescriptor.getUri()));
		// we verify the handler state (isDisposed() must return false)
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				firstHandlerCodeDescriptor, "isDisposed", null, boolean.class,
				null)));
		action.setUri(PLUGIN_PLATFORM_TEST_URI);
		// we verify the handler state (isDisposed() must return true)
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				firstHandlerCodeDescriptor, "isDisposed", null, boolean.class,
				null)));

		AbstractCodeDescriptor secondHandlerCodeDescriptor = actionAdapter.getCodeDescriptor();
		assertTrue(secondHandlerCodeDescriptor != null);
		assertTrue(secondHandlerCodeDescriptor instanceof PluginCodeDescriptor);

		// since SecondHandler is defined using a platform:/plugin/ we need to
		// set the internal bundle variable
		ReflectionUtils.setPrivateField(secondHandlerCodeDescriptor,
				"resolvedBundle", bundle);

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(secondHandlerCodeDescriptor,
				"execute", new Class[] { int.class, int.class }, int.class,
				new Object[] { 2, 4 });
		assertTrue(result instanceof Integer);
		assertTrue(((Integer) result).intValue() == 2 + 4);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				secondHandlerCodeDescriptor, "isDisposed", null, boolean.class,
				null)));
		action.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				secondHandlerCodeDescriptor, "isDisposed", null, boolean.class,
				null)));

		// we check that the CodeDescriptor attached to the deferredAdapter has
		// been set to null
		assertTrue(actionAdapter.getCodeDescriptor() == null);

		// we set the uri another time
		action.setUri(PLUGIN_PLATFORM_TEST_URI);
		secondHandlerCodeDescriptor = actionAdapter.getCodeDescriptor();
		assertTrue(secondHandlerCodeDescriptor != null);
		assertTrue(actionAdapter.getCodeDescriptor() instanceof PluginCodeDescriptor);
		// since SecondHandler is defined using a platform:/plugin/ we need to
		// set the internal bundle variable
		ReflectionUtils.setPrivateField(actionAdapter.getCodeDescriptor(),
				"resolvedBundle", bundle);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				secondHandlerCodeDescriptor, "isDisposed", null, boolean.class,
				null)));
		// this time we test with an empty string instead of a null value
		action.setUri(""); //$NON-NLS-1$
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				secondHandlerCodeDescriptor, "isDisposed", null, boolean.class,
				null)));
		OsgiUtils.stopOsgiFwk();
	}

	@Test
	public void testRemoveAdapterFromModel() {
		Action model = EDPHandlersFactory.eINSTANCE.createAction();;
		model.setUri(BASIC_ACTION_HANDLER);
		ActionAdapter actionAdapter = new ActionAdapter();
		model.eAdapters().add(actionAdapter);
		AbstractCodeDescriptor codeDescriptor = actionAdapter.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_ACTION_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				actionAdapter.getCodeDescriptor(), "execute", new Class[] {
						int.class, int.class }, int.class,
				new Object[] { 2, 4 });
		assertTrue(result instanceof Integer);
		assertTrue(((Integer) result).intValue() == 2 - 4);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));
		model.eAdapters().remove(actionAdapter);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(codeDescriptor,
				"isDisposed", null, boolean.class, null)));

	}

}
