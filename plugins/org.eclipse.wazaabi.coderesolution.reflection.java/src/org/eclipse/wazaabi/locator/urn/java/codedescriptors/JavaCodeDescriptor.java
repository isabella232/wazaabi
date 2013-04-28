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

package org.eclipse.wazaabi.coderesolution.reflection.java.codedescriptors;

import java.lang.reflect.Method;

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractMethodInvoker;

public class JavaCodeDescriptor extends AbstractCodeDescriptor {

	private boolean instanciated = false;

	public static class JavaMethodDescriptor extends
			AbstractCodeDescriptor.MethodDescriptor {

		private final Method javaMethod;

		protected Method getJavaMethod() {
			return javaMethod;
		}

		public JavaMethodDescriptor(String methodName, String[] parameterNames,
				Class<?>[] parameterTypes, Class<?> returnType,
				Method javaMethod) {
			super(methodName, parameterNames, parameterTypes, returnType);
			this.javaMethod = javaMethod;
		}

	};

	private final static JavaMethodInvoker methodInvoker = new JavaMethodInvoker();
	private final String javaClassName;
	private Object instance = null;

	public String getJavaClassName() {
		return javaClassName;
	}

	private Class<?> resolvedClass = null;

	protected Class<?> getResolvedClass() {
		return resolvedClass;
	}

	public JavaCodeDescriptor(String javaClassName) {
		this.javaClassName = javaClassName;
	}

	@Override
	protected AbstractMethodInvoker getMethodInvoker() {
		return methodInvoker;
	}

	protected Class<?> resolveClass() {
		try {
			return getClass().getClassLoader().loadClass(getJavaClassName());
		} catch (ClassNotFoundException e) {
			// log this
			System.err.println(getUri());
			System.err.println("unable to load class: " + getJavaClassName());
			// e.printStackTrace();
		}
		return null;
	}

	protected Object makeNewInstance() {
		if (resolvedClass == null)
			resolvedClass = resolveClass();
		if (resolvedClass != null) {
			try {
				Object result = resolvedClass.newInstance();
				instanciated = true;
				return result;
			} catch (InstantiationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return null;
	}

	protected Object getInstance() {
		if (instance == null)
			instance = makeNewInstance();
		return instance;
	}

	@Override
	public MethodDescriptor getMethodDescriptor(String methodName,
			String[] parameterNames, Class<?>[] parameterTypes,
			Class<?> returnType) {

		if (methodName == null || "".equals(methodName)) //$NON-NLS-1$
			return null;

		if (parameterTypes == null)
			parameterTypes = new Class[0];

		if (returnType == null)
			returnType = Void.TYPE;

		if (resolvedClass == null)
			resolvedClass = resolveClass();

		Class<?> clazz = resolvedClass;
		try {
			while (clazz != null) {
				for (Method method : clazz.getDeclaredMethods())
					if (methodName.equals(method.getName())
							&& areParameterTypesCompatible(parameterTypes,
									method.getParameterTypes())
							&& returnType.isAssignableFrom(method
									.getReturnType()))
						return new JavaMethodDescriptor(methodName,
								parameterNames, parameterTypes, returnType,
								method);
				clazz = clazz.getSuperclass();
			}
		} catch (Throwable e) {
			// TODO log this
			System.err.println("error while getting declared methods in class "
					+ resolvedClass);
			e.printStackTrace();
		}

		return null;
	}

	/**
	 * Returns true if two arrays of Class are compatible. Compatible means that
	 * the two arrays have the same length, and each class of expected
	 * parameters is assignable from proposed parameters.
	 * 
	 * @param expectedParameterTypes
	 * @param actualParameterTypes
	 * @return
	 */
	private boolean areParameterTypesCompatible(
			Class<?>[] expectedParameterTypes, Class<?>[] actualParameterTypes) {
		if (expectedParameterTypes.length == 0)
			if (actualParameterTypes == null
					|| actualParameterTypes.length == 0)
				return true;
			else
				return false;

		if (expectedParameterTypes.length != actualParameterTypes.length)
			return false;

		for (int i = 0; i < expectedParameterTypes.length; i++)
			if (!expectedParameterTypes[i]
					.isAssignableFrom(actualParameterTypes[i]))
				return false;
		return true;
	}

	@Override
	public boolean isInstanciated() {
		return instanciated;
	}
}
