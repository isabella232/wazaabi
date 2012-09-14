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

package org.eclipse.wazaabi.engine.core.tests.osgi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.beans.MethodDescriptor;

import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.tests.OsgiUtils;
import org.eclipse.wazaabi.engine.edp.tests.ReflectionUtils;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;

public class TestCodeDescriptorResolutionInOSGIMode {

	public static final String TEST_PLUGIN_FILE_NAME = "org.eclipse.wazaabi.test.testbundle_1.0.0.jar";//$NON-NLS-1$
	public static final String TEST_PLUGIN_URI_BUNDLE_SYMBOLIC_NAME = "org.eclipse.wazaabi.test.testbundle"; //$NON-NLS-1$
	public static final String TEST_PLUGIN_URI_CLASS_NAME = "org.eclipse.wazaabi.test.testbundle.handlers.FirstTestHandler"; //$NON-NLS-1$

	public static final String eclipseDependencies[] = {
			"org.eclipse.osgi.services_3.2.100.v20100503.jar", //$NON-NLS-1$
			"org.eclipse.equinox.util_1.0.200.v20100503.jar", //$NON-NLS-1$
			"org.eclipse.equinox.ds_1.2.0.v20100507.jar", //$NON-NLS-1$
			"org.eclipse.equinox.common_3.6.0.v20100503.jar", //$NON-NLS-1$
			"org.eclipse.core.jobs_3.5.0.v20100515.jar", //$NON-NLS-1$
			"org.eclipse.equinox.registry_3.5.0.v20100503.jar", //$NON-NLS-1$
			"org.eclipse.equinox.preferences_3.3.0.v20100503.jar", //$NON-NLS-1$
			"org.eclipse.core.contenttype_3.4.100.v20100505-1235.jar", //$NON-NLS-1$
			"org.eclipse.equinox.app_1.3.0.v20100512.jar", //$NON-NLS-1$
			"org.eclipse.core.runtime_3.6.0.v20100505.jar", //$NON-NLS-1$
			"org.eclipse.emf.common_2.6.0.v20100614-1136.jar", //$NON-NLS-1$
			"org.eclipse.emf.ecore_2.6.0.v20100614-1136.jar" }; //$NON-NLS-1$

	public static final String wazaabiDependencies[] = {
			"org.eclipse.wazaabi.mm.edp_1.0.0.jar", //$NON-NLS-1$
			"org.eclipse.wazaabi.engine.edp_1.0.0.jar", //$NON-NLS-1$
			"org.eclipse.wazaabi.coderesolution.reflection.java_1.0.0.jar", //$NON-NLS-1$
			"org.eclipse.wazaabi.coderesolution.reflection.java.plugins_1.0.0.jar" }; //$NON-NLS-1$

	public static final String TEST_PLUGIN_URI = "platform:/plugin/"
			+ TEST_PLUGIN_URI_BUNDLE_SYMBOLIC_NAME
			+ "/" + TEST_PLUGIN_URI_CLASS_NAME; //$NON-NLS-1$ 

	@Test
	public void testPlatformPluginResolution() {
		// TODO : here we add and remove code resolver and observe/test
	}

	@Test
	public void testPluginLifeCycle() {
		// TODO : here we add and remove the example plugin and observe/test
	}

	@Test
	public void testPlatformPluginExecution() {

		// TODO : test constructors and dispose method

		// start the osgi framework
		BundleContext context = OsgiUtils.launchOsgiFwk();
		assertTrue(context != null);

		// install and start eclipse dependencies
		for (String bundle : eclipseDependencies)
			OsgiUtils.installAndStartBundle(context, bundle, getClass()
					.getClassLoader(), bundle);

		// install and start wazaabi dependencies
		for (String bundle : wazaabiDependencies)
			OsgiUtils.installAndStartBundle(context, bundle, getClass()
					.getClassLoader(), bundle);

		/* Bundle bundle = */OsgiUtils.installAndStartBundle(context,
				TEST_PLUGIN_FILE_NAME, getClass().getClassLoader(),
				TEST_PLUGIN_FILE_NAME);

		ServiceReference<?> serviceReference = context
				.getServiceReference("org.eclipse.wazaabi.engine.edp.Registry"); //$NON-NLS-1$
		assertNotNull(serviceReference);
		Object registry = context.getService(serviceReference);
		assertNotNull(registry);
		AbstractCodeDescriptor codeDescriptor = EDPSingletons.getComposedCodeLocator().resolveCodeDescriptor(TEST_PLUGIN_FILE_NAME);

		//Class<?> codeResolverClass = serviceReference.getBundle().loadClass(CodeResolver.class.getName());
		//Class<?> codeResolverClass = serviceReference.getBundle().loadClass(EDPSingletons.getComposedCodeLocator().resolveCodeDescriptor(TEST_PLUGIN_FILE_NAME).toString());

		// get the code descriptor using the registry (from a private static
		// CodeResolver's method)
//			Object codeDescriptor = ReflectionUtils.invokeStaticPrivateMethod(
//					codeResolverClass, "resolveCodeDescriptor", new String[] { //$NON-NLS-1$
//					Registry.class.getName(), String.class.getName() },
//					AbstractCodeDescriptor.class.getName(), new Object[] {
//							registry, TEST_PLUGIN_URI });
//			Object result = ReflectionUtils.invokeMethod(
//					codeDescriptor, "resolveCodeDescriptor", new String[] {
//							Registry.class.getName(), String.class.getName() },
//							new AbstractCodeDescriptor,
//							new Object[] {registry, TEST_PLUGIN_URI });
		
		assertNotNull(codeDescriptor);

		// test if the code descriptor is a PluginCodeDescriptor
		assertEquals(
				codeDescriptor.getClass().getName(),
				"org.eclipse.wazaabi.coderesolution.reflection.java.plugins.codedescriptors.PluginCodeDescriptor"); //$NON-NLS-1$

		// test if the java class name associated to the plugin code
		// descriptor is the expected one.
		assertEquals(TEST_PLUGIN_URI_CLASS_NAME,
				ReflectionUtils.invokePublicMethod(codeDescriptor,
						"getJavaClassName", new String[] {},
						String.class.getName(), new Object[] {}));

		// test if the bundle sympolic name associated to the plugin code
		// descriptor is the expected one.
		assertEquals(TEST_PLUGIN_URI_BUNDLE_SYMBOLIC_NAME,
				ReflectionUtils.invokePublicMethod(codeDescriptor,
						"getBundleSymbolicName", new String[] {},
						String.class.getName(), new Object[] {}));

		Object result = null;

		// invokes execute(2,4)
		result = invokeMethod(codeDescriptor, "execute", new Class<?>[] {
				int.class, int.class }, int.class, new Object[] { 2, 4 });

		// test the result (expected 6)
		assertTrue(result instanceof Integer);
		assertEquals(6, ((Integer) result).intValue());

		// invokes execute("hello")
		result = invokeMethod(codeDescriptor, "execute",
				new Class<?>[] { String.class }, String.class,
				new Object[] { "hello" });

		// test the result (expected "hello")
		assertTrue(result instanceof String);
		assertEquals(
				"hello:succeed(org.eclipse.wazaabi.test.testbundle.handlers.FirstTestHandler)", result); //$NON-NLS-1$

		OsgiUtils.stopOsgiFwk();
	}

	/**
	 * Invokes the method whose signature is specified by the methodName,
	 * parameterTypes, returnedValue on the given code descriptor giving the
	 * parameters as arguments.
	 * 
	 * @param codeDescriptor
	 * @param methodName
	 * @param parameterTypes
	 * @param returnedValue
	 * @param parameters
	 * @return
	 */
	protected Object invokeMethod(Object codeDescriptor, String methodName,
			Class<?> parameterTypes[], Class<?> returnedValue,
			Object[] parameters) {
		// get the code descriptor
		Object methodDescriptor = ReflectionUtils.invokePublicMethod(
				codeDescriptor,
				"getMethodDescriptor",
				new String[] { String.class.getName(),
						(new Class<?>[] {}).getClass().getName(),
						Class.class.getName() },
				MethodDescriptor.class.getName(), new Object[] { methodName,
						parameterTypes, returnedValue });

		assertNotNull(methodDescriptor);
		// test if the method descriptor is the expected one
		assertEquals(
				"org.eclipse.wazaabi.coderesolution.reflection.java.codedescriptors.JavaCodeDescriptor$JavaMethodDescriptor", //$NON-NLS-1$
				methodDescriptor.getClass().getName());

		return ReflectionUtils
				.invokePublicMethod(
						codeDescriptor,
						"invokeMethod", //$NON-NLS-1$
						new String[] {
								"org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor$MethodDescriptor", //$NON-NLS-1$
								(new Object[] {}).getClass().getName() },
						Object.class.getName(), new Object[] {
								methodDescriptor, parameters });
	}

}
