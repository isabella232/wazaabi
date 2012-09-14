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
import org.eclipse.wazaabi.engine.edp.adapters.ValidatorAdapter;
import org.eclipse.wazaabi.engine.edp.bundled.validators.TestBundledBasicValidator;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.tests.ReflectionUtils;
import org.eclipse.wazaabi.engine.edp.validators.BundledValidator;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.Validator;
import org.junit.Test;

public class TestValidatorAdapter extends AbstractTestOperationAdapter {

	public static final String BASIC_VALIDATOR_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BasicValidatorHandler"; //$NON-NLS-1$ 
	public static final String BAD_VALIDATOR_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BadValidatorHandler"; //$NON-NLS-1$
	public static final String BASIC_VALIDATOR_ID = "testBundledBasicValidator";

	@Test
	public void testAddAdapterToModel() {
		Validator validator = EDPHandlersFactory.eINSTANCE.createValidator();
		validator.setUri(BASIC_VALIDATOR_HANDLER);
		ValidatorAdapter validatorAdapter = new ValidatorAdapter();
		validator.eAdapters().add(validatorAdapter);
		AbstractCodeDescriptor codeDescriptor = validatorAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_VALIDATOR_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				validatorAdapter.getCodeDescriptor(), "isValid", new Class[] {
						int.class, int.class }, boolean.class, new Object[] {
						2, 2 });
		assertTrue(result instanceof Boolean);
		assertTrue(((Boolean) result).booleanValue() == true);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
		validator.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
	}

	@Test
	public void testAddAdapterToModelWithException() {
		Validator validator = EDPHandlersFactory.eINSTANCE.createValidator();
		validator.setUri(BAD_VALIDATOR_HANDLER);
		ValidatorAdapter validatorAdapter = new ValidatorAdapter();
		validator.eAdapters().add(validatorAdapter);
		AbstractCodeDescriptor codeDescriptor = validatorAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BAD_VALIDATOR_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = null;
		try {
			result = ReflectionUtils.invokeMethod(
					validatorAdapter.getCodeDescriptor(), "isValid",
					new Class[] { int.class, int.class }, boolean.class,
					new Object[] { 2, 2 });
		} catch (RuntimeException e) {
			Assert.assertTrue(e.getCause() instanceof OperationAborted);
		}
		Assert.assertNull(result);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
		validator.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
	}

	@Test
	public void testIsAdapterForTypeObject() {
		ValidatorAdapter validatorAdapter = new ValidatorAdapter();
		assertTrue(validatorAdapter
				.isAdapterForType(EDPHandlersFactory.eINSTANCE
						.createValidator()));
	}

	@Test
	public void testRemoveAdapterFromModel() {
		Validator model = EDPHandlersFactory.eINSTANCE.createValidator();
		model.setUri(BASIC_VALIDATOR_HANDLER);
		ValidatorAdapter validatorAdapter = new ValidatorAdapter();
		model.eAdapters().add(validatorAdapter);
		AbstractCodeDescriptor codeDescriptor = validatorAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_VALIDATOR_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				validatorAdapter.getCodeDescriptor(), "isValid", new Class[] {
						int.class, int.class }, boolean.class, new Object[] {
						2, 4 });
		assertTrue(result instanceof Boolean);
		assertTrue(((Boolean) result).booleanValue() == false);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
		model.eAdapters().remove(validatorAdapter);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));

	}

	@Test
	public void testAddAdapterToModelbyOSGiDS() {
		Validator validator = EDPHandlersFactory.eINSTANCE.createValidator();
		validator.setId(BASIC_VALIDATOR_ID);
		ValidatorAdapter validatorAdapter = new ValidatorAdapter();
		validator.eAdapters().add(validatorAdapter);
		BundledValidator bundledValidator = validatorAdapter
				.getInnerBundledValidator();

		assertTrue(bundledValidator instanceof BundledValidator);
		assertTrue(bundledValidator instanceof TestBundledBasicValidator);
		Object result = bundledValidator.validate(null, null);

		assertTrue(result instanceof Boolean);
		assertTrue((Boolean) result);

		// // at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(bundledValidator.isDisposed()));
		validator.setId(null);
		// // now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(bundledValidator.isDisposed()));
	}

	@Test
	public void testMixOSGiDSAndDeferred() {
		Validator validator = EDPHandlersFactory.eINSTANCE.createValidator();
		validator.setId(BASIC_VALIDATOR_ID);
		validator.setUri(BASIC_VALIDATOR_HANDLER);
		ValidatorAdapter validatorAdapter = new ValidatorAdapter();
		validator.eAdapters().add(validatorAdapter);
		BundledValidator bundledValidator = validatorAdapter
				.getInnerBundledValidator();

		assertTrue(bundledValidator instanceof BundledValidator);
		assertTrue(bundledValidator instanceof TestBundledBasicValidator);
		Object result = bundledValidator.validate(null, null);

		assertTrue(result instanceof Boolean);
		assertTrue((Boolean) result);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(bundledValidator.isDisposed()));
		validator.setId(null);
		// now, the secondHandler must have been disposed
		// assertTrue(Boolean.TRUE.equals(bundledConverter.isDisposed()));

		AbstractCodeDescriptor codeDescriptor = validatorAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_VALIDATOR_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		result = ReflectionUtils.invokeMethod(
				validatorAdapter.getCodeDescriptor(), "isValid", new Class[] {
						int.class, int.class }, boolean.class, new Object[] {
						2, 2 });
		assertTrue(result instanceof Boolean);
		assertTrue(((Boolean) result).booleanValue() == true);

	}
}
