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

package org.eclipse.wazaabi.locator.urn.java.codedescriptors;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractMethodInvoker;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor.MethodDescriptor;
import org.eclipse.wazaabi.engine.edp.exceptions.OperationAborted;
import org.eclipse.wazaabi.locator.urn.java.codedescriptors.JavaCodeDescriptor.JavaMethodDescriptor;

public class JavaMethodInvoker extends AbstractMethodInvoker {

	@Override
	public Object invokeMethod(AbstractCodeDescriptor codeDescriptor,
			MethodDescriptor methodDescriptor, Object[] parameterValues) {
		if (methodDescriptor instanceof JavaMethodDescriptor
				&& codeDescriptor instanceof JavaCodeDescriptor
				&& ((JavaMethodDescriptor) methodDescriptor).getJavaMethod() != null
				&& ((JavaCodeDescriptor) codeDescriptor).getInstance() != null) {

			try {
				return ((JavaMethodDescriptor) methodDescriptor)
						.getJavaMethod()
						.invoke(((JavaCodeDescriptor) codeDescriptor)
								.getInstance(),
								parameterValues);
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				if (e.getTargetException() instanceof OperationAborted) {
					throw new RuntimeException(e.getTargetException());
				} else {
					e.printStackTrace();
				}
			}
		}
		return null;
	}
}
