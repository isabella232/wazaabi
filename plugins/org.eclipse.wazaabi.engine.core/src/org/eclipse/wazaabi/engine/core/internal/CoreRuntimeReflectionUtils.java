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

package org.eclipse.wazaabi.engine.core.internal;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Wazaabi core engine is able to be launched in several environments or
 * platforms. It could be ran inside a RCP application or from a pure java main
 * not related to any osgi framework. This package makes it possible to remove
 * any dependencies to org.eclipse.core.runtime by using java reflection when
 * the need of org.eclipse.core.runtime is present.
 * 
 * @author Olivier Moises
 * 
 */
public class CoreRuntimeReflectionUtils {

	/**
	 * Invokes/call a method.
	 * 
	 * @param caller
	 * @param classInstance
	 * @param className
	 * @param methodName
	 * @param parameterClasses
	 * @param parameters
	 * @return The result of the called method or null (if the call returns null
	 *         or if the method could not be called).
	 * 
	 */
	public static Object invokeMethod(Object caller, Object classInstance,
			String className, String methodName, String parameterClasses[],
			Object[] parameters) {
		Object result = null;
		try {
			Class<?> clazz = caller.getClass().getClassLoader().loadClass(
					className);
			Class<?> paramClasses[] = new Class<?>[0];
			if (parameterClasses != null && parameterClasses.length > 0) {
				paramClasses = new Class<?>[parameterClasses.length];
				for (int i = 0; i < parameterClasses.length; i++)
					paramClasses[i] = caller.getClass().getClassLoader()
							.loadClass(parameterClasses[i]);
			}
			if (parameters == null)
				parameters = new Object[0];
			Method method = clazz.getMethod(methodName, paramClasses);
			result = method.invoke(classInstance, parameters);
		} catch (ClassNotFoundException e) {
			// NOTHING TO DO HERE, NO PROBS
		} catch (SecurityException e) {
			// when launched in a non OSGI context, the SecurityException means
			// that the platform is not running
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// NOTHING TO DO HERE, NO PROBS
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		}
		return result;
	}

}
