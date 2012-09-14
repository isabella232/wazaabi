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

package org.eclipse.wazaabi.engine.edp.tests;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;

public class ReflectionUtils {

	/**
	 * Sets a private field unsig java reflection.
	 * 
	 * @param target
	 *            The object owning the field to be set.
	 * @param privateFieldName
	 *            The name of the field to set.
	 * @param newValue
	 *            The future field value.
	 */
	public static void setPrivateField(Object target, String privateFieldName,
			Object newValue) {
		Field privateField;
		try {
			privateField = target.getClass().getDeclaredField(privateFieldName);
			privateField.setAccessible(true);
			privateField.set(target, newValue);
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchFieldException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Shortcut allowing execution of a CodeDescriptor Method while checking the
	 * existence of the method.
	 * 
	 * @param codeDescriptor
	 * @param methodName
	 * @param parameterTypes
	 * @param returnType
	 * @param parameterValues
	 * @return
	 */
	public static Object invokeMethod(AbstractCodeDescriptor codeDescriptor,
			String methodName, Class<?>[] parameterTypes, Class<?> returnType,
			Object[] parameterValues) {
		assertTrue(codeDescriptor != null);
		String[] parameterNames = new String[] { "eventDispatcher", "eventHandler", "event" };
		AbstractCodeDescriptor.MethodDescriptor methodDescriptor = codeDescriptor
				.getMethodDescriptor(methodName, parameterNames , parameterTypes, returnType);
		assertTrue(methodDescriptor != null);
		return codeDescriptor.invokeMethod(methodDescriptor, parameterValues);
	}

	/**
	 * Invoke a static private method on the given class. Due to
	 * <code>ClassLoader</code> differences, types must be given as string
	 * instead of java classes.
	 * 
	 * @param clazz
	 *            The java.lang.class of the type on which invoke the method, in
	 *            osgi model, the class must be attached to the class loader
	 *            which owns it (not the JUnit classloader). The classloader
	 *            must be compatible with the parameters classloader too.
	 * @param methodName
	 *            The method to invoke
	 * @param parameterTypeNames
	 *            A array of java.lang.String, can be null or empty
	 * @param returnTypeName
	 *            The name of the returned type
	 * @param parameterValues
	 *            The values of the parameters.
	 * @return The result of the invocation of the method.
	 * 
	 * @see #getDeclaredMethod(Class, String, String[], String)
	 */
	public static Object invokeStaticPrivateMethod(Class<?> clazz,
			String methodName, String[] parameterTypeNames,
			String returnTypeName, Object[] parameterValues) {
		Method method = getDeclaredMethod(clazz, methodName,
				parameterTypeNames, returnTypeName);
		if (method != null) {
			try {
				method.setAccessible(true);
				return method.invoke(null, parameterValues);
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
			return null;
		} else
			throw new RuntimeException("Declared method " + methodName
					+ " not found in " + clazz);
	}

	/**
	 * Invoke a public method on the given class. Due to
	 * <code>ClassLoader</code> issues, types must be given as string instead of
	 * java classes.
	 * 
	 * @param target
	 *            The object on which invoke the method, in osgi model, the
	 *            object is given by the plugin owning it and must be attached
	 *            to a classloader compatible with the parameters one.
	 * @param methodName
	 *            The method to invoke
	 * @param parameterTypeNames
	 *            A array of java.lang.String, can be null or empty
	 * @param returnTypeName
	 *            The name of the returned type
	 * @param parameterValues
	 *            The values of the parameters.
	 * @return The result of the invocation of the method.
	 * 
	 * @see #getDeclaredMethod(Class, String, String[], String)
	 */
	public static Object invokePublicMethod(Object target, String methodName,
			String[] parameterTypeNames, String returnTypeName,
			Object[] parameterValues) {
		assertNotNull(target);

		Method method = getMethod(target.getClass(), methodName,
				parameterTypeNames, returnTypeName);
		if (method != null) {
			try {
				return method.invoke(target, parameterValues);
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
			return null;
		} else
			throw new RuntimeException("Declared method " + methodName
					+ " not found in " + target.getClass());
	}

	/**
	 * Returns a method found in the declared methods (@see
	 * {@link java.lang.Class#getDeclaredMethods()} of the given class. In order
	 * to avoid <code>ClassLoader</code> issues, type names are given instead of
	 * <code>java.lang.Class</code>.
	 * 
	 * @param clazz
	 *            The java.lang.class of the type on which invoke the method, in
	 *            osgi model, the class must be attached to the classloader of
	 *            the bundle which owns it (not the JUnit classloader).
	 * @param methodName
	 *            The method to invoke
	 * @param parameterTypeNames
	 *            A array of java.lang.String, can be null or empty
	 * @param returnTypeName
	 *            The name of the returned type
	 * @return A <code>java.lang.reflect.method</code> if found, null otherwise.
	 */
	public static Method getDeclaredMethod(Class<?> clazz, String methodName,
			String[] parameterTypeNames, String returnTypeName) {
		assertTrue(clazz != null);
		assertNotNull(methodName);
		assertTrue(!methodName.equals("")); //$NON-NLS-1$
		if (parameterTypeNames == null)
			parameterTypeNames = new String[0];
		for (Method method : clazz.getDeclaredMethods()) {
			if (methodName.equals(method.getName())) {
				if (parameterTypeNames.length != method.getParameterTypes().length)
					continue;
				if (returnTypeName == null && method.getReturnType() != null)
					continue;
				if (returnTypeName != null && method.getReturnType() == null)
					continue;
				if (returnTypeName != null
						&& method.getReturnType() != null
						&& !returnTypeName.equals(method.getReturnType()
								.getName()))
					continue;

				boolean areSameTypes = true;
				for (int i = 0; i < parameterTypeNames.length; i++) {
					if (!method.getParameterTypes()[i].getName().equals(
							parameterTypeNames[i])) {
						areSameTypes = false;
						break;
					}
				}
				if (areSameTypes)
					return method;
			}
		}
		return null;
	}

	/**
	 * Returns a method found in the methods (@see
	 * {@link java.lang.Class#getMethods()} of the given class. In order to
	 * avoid <code>ClassLoader</code> issues, type names are given instead of
	 * <code>java.lang.Class</code>.
	 * 
	 * @param clazz
	 *            The java.lang.class of the type on which invoke the method, in
	 *            osgi model, the class must be attached to the classloader of
	 *            the bundle which owns it (not the JUnit classloader).
	 * @param methodName
	 *            The method to invoke
	 * @param parameterTypeNames
	 *            A array of java.lang.String, can be null or empty
	 * @param returnTypeName
	 *            The name of the returned type
	 * @return A <code>java.lang.reflect.method</code> if found, null otherwise.
	 */
	public static Method getMethod(Class<?> clazz, String methodName,
			String[] parameterTypeNames, String returnTypeName) {
		assertTrue(clazz != null);
		assertNotNull(methodName);
		assertTrue(!methodName.equals("")); //$NON-NLS-1$
		if (parameterTypeNames == null)
			parameterTypeNames = new String[0];
		for (Method method : clazz.getMethods()) {
			if (methodName.equals(method.getName())) {
				if (parameterTypeNames.length != method.getParameterTypes().length)
					continue;
				if (returnTypeName == null && method.getReturnType() != null)
					continue;
				if (returnTypeName != null && method.getReturnType() == null)
					continue;
				if (returnTypeName != null
						&& method.getReturnType() != null
						&& !returnTypeName.equals(method.getReturnType()
								.getName()))
					continue;

				boolean areSameTypes = true;
				for (int i = 0; i < parameterTypeNames.length; i++) {
					if (!method.getParameterTypes()[i].getName().equals(
							parameterTypeNames[i])) {
						areSameTypes = false;
						break;
					}
				}
				if (areSameTypes)
					return method;
			}
		}
		return null;
	}

}
