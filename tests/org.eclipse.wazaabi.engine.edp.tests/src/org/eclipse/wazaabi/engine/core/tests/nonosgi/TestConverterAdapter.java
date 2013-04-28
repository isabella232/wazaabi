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
import static org.junit.Assert.assertNotNull;

import java.lang.reflect.Field;

import junit.framework.Assert;

import org.eclipse.wazaabi.engine.edp.adapters.ConverterAdapter;
import org.eclipse.wazaabi.engine.edp.bundled.converters.TestBundledBasicConverter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.converters.BundledConverter;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.engine.edp.tests.ReflectionUtils;
import org.eclipse.wazaabi.locator.platform.plugin.codedescriptors.PluginCodeDescriptor;
import org.eclipse.wazaabi.locator.urn.java.codedescriptors.JavaCodeDescriptor;
import org.eclipse.wazaabi.mm.edp.handlers.Converter;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.junit.Test;

public class TestConverterAdapter extends AbstractTestOperationAdapter {

	public static final String BASIC_CONVERTER_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BasicConverterHandler"; //$NON-NLS-1$ 
	public static final String BAD_CONVERTER_HANDLER = "urn:java:org.eclipse.wazaabi.engine.edp.tests.handlers.BadConverterHandler"; //$NON-NLS-1$
	public static final String BASIC_CONVERTER_HANDLER_ID = "TestBasicConverter";

	@Test
	public void testAddAdapterToModel() {
		Converter converter = EDPHandlersFactory.eINSTANCE.createConverter();
		converter.setUri(BASIC_CONVERTER_HANDLER);
		ConverterAdapter converterAdapter = new ConverterAdapter();
		converter.eAdapters().add(converterAdapter);
		AbstractCodeDescriptor codeDescriptor = converterAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_CONVERTER_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				converterAdapter.getCodeDescriptor(), "convert",
				new Class[] { int.class }, int.class, new Object[] { 2 });
		assertTrue(result instanceof Integer);
		assertTrue(((Integer) result) == 7);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
		converter.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
	}

	@Test
	public void testAddAdapterToModelWithException() {
		Converter converter = EDPHandlersFactory.eINSTANCE.createConverter();
		converter.setUri(BAD_CONVERTER_HANDLER);
		ConverterAdapter converterAdapter = new ConverterAdapter();
		converter.eAdapters().add(converterAdapter);
		AbstractCodeDescriptor codeDescriptor = converterAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BAD_CONVERTER_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = null;
		try {
			result = ReflectionUtils.invokeMethod(
					converterAdapter.getCodeDescriptor(), "convert",
					new Class[] { int.class }, int.class, new Object[] { 2 });
		} catch (RuntimeException e) {
			Assert.assertTrue(e.getCause() instanceof OperationAborted);
		}
		Assert.assertNull(result);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
		converter.setUri(null);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
	}

	@Test
	public void testIsAdapterForTypeObject() {
		ConverterAdapter converterAdapter = new ConverterAdapter();
		assertTrue(converterAdapter
				.isAdapterForType(EDPHandlersFactory.eINSTANCE
						.createConverter()));
	}

	@Test
	public void testRemoveAdapterFromModel() {
		Converter model = EDPHandlersFactory.eINSTANCE.createConverter();
		model.setUri(BASIC_CONVERTER_HANDLER);
		ConverterAdapter converterAdapter = new ConverterAdapter();
		model.eAdapters().add(converterAdapter);
		AbstractCodeDescriptor codeDescriptor = converterAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_CONVERTER_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		Object result = ReflectionUtils.invokeMethod(
				converterAdapter.getCodeDescriptor(), "convert",
				new Class[] { int.class }, int.class, new Object[] { 2 });
		assertTrue(result instanceof Integer);
		assertTrue((Integer) result == 7);

		// at this time, the secondHandler must not have been disposed
		assertTrue(Boolean.FALSE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));
		model.eAdapters().remove(converterAdapter);
		// now, the secondHandler must have been disposed
		assertTrue(Boolean.TRUE.equals(ReflectionUtils.invokeMethod(
				codeDescriptor, "isDisposed", null, boolean.class, null)));

	}

	@Test
	public void testAddAdapterToModelbyOSGiDS() {
		Converter converter = EDPHandlersFactory.eINSTANCE.createConverter();
		converter.setId(BASIC_CONVERTER_HANDLER_ID);
		ConverterAdapter converterAdapter = new ConverterAdapter();
		converter.eAdapters().add(converterAdapter);
		BundledConverter bundledConverter = getBundledConverter(converterAdapter);
		assertNotNull(bundledConverter);

		assertTrue(bundledConverter instanceof BundledConverter);
		assertTrue(bundledConverter instanceof TestBundledBasicConverter);
		Object result = bundledConverter.convert(2);

		assertTrue(result instanceof Integer);
		assertTrue(((Integer) result) == 8);

		// // at this time, the secondHandler must not have been disposed
		// assertTrue(Boolean.FALSE.equals(bundledConverter.isDisposed()));
		// converter.setId(null);
		// // now, the secondHandler must have been disposed
		// assertTrue(Boolean.TRUE.equals(bundledConverter.isDisposed()));
	}

	@Test
	public void testMixOSGiDSAndDeferred() {
		Converter converter = EDPHandlersFactory.eINSTANCE.createConverter();
		converter.setId(BASIC_CONVERTER_HANDLER_ID);
		converter.setUri(BASIC_CONVERTER_HANDLER);
		ConverterAdapter converterAdapter = new ConverterAdapter();
		converter.eAdapters().add(converterAdapter);
		BundledConverter bundledConverter = getBundledConverter(converterAdapter);
		assertNotNull(bundledConverter);

		assertTrue(bundledConverter instanceof BundledConverter);
		assertTrue(bundledConverter instanceof TestBundledBasicConverter);
		Object result = bundledConverter.convert(2);

		assertTrue(result instanceof Integer);
		assertTrue(((Integer) result) == 8);

		// // at this time, the secondHandler must not have been disposed
		// assertTrue(Boolean.FALSE.equals(bundledConverter.isDisposed()));
		converter.setId(null);
		// now, the secondHandler must have been disposed
		// assertTrue(Boolean.TRUE.equals(bundledConverter.isDisposed()));

		AbstractCodeDescriptor codeDescriptor = converterAdapter
				.getCodeDescriptor();

		assertTrue(codeDescriptor instanceof JavaCodeDescriptor);
		assertFalse(codeDescriptor instanceof PluginCodeDescriptor);
		assertTrue(BASIC_CONVERTER_HANDLER.equals(codeDescriptor.getUri()));

		// test <code>public int execute(int a, int b)</code>
		result = ReflectionUtils.invokeMethod(
				converterAdapter.getCodeDescriptor(), "convert",
				new Class[] { int.class }, int.class, new Object[] { 2 });
		assertTrue(result instanceof Integer);
		assertTrue(((Integer) result) == 7);

	}

	protected BundledConverter getBundledConverter(
			ConverterAdapter converterAdapter) {
		try {
			Field privateStringField = ConverterAdapter.class
					.getDeclaredField("bundledConverter"); //$NON-NLS-1$
			privateStringField.setAccessible(true);
			return (BundledConverter) privateStringField.get(converterAdapter);
		} catch (NoSuchFieldException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		}
		return null;
	}

}
