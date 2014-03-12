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
 *   Pavel Erofeev - locator for platform:/resource URIs
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.outline;

import org.eclipse.wazaabi.locator.urn.java.codedescriptors.JavaCodeDescriptor;

public class IDECodeDescriptor extends JavaCodeDescriptor {

	private final String bundleSymbolicName;

	public IDECodeDescriptor(String bundleSymbolicName,
			String javaClassName) {
		super(javaClassName);
		this.bundleSymbolicName = bundleSymbolicName;
	}

	public String getBundleSymbolicName() {
		return bundleSymbolicName;
	}

	@Override
	protected Class<?> resolveClass() {
		throw new IllegalStateException(
				"ResourceCodeDescriptor does not support class resolution");
	}

	@Override
	public MethodDescriptor getMethodDescriptor(String methodName,
			String[] parameterNames, Class<?>[] parameterTypes,
			Class<?> returnType) {
		// this code locator never creates any method descriptor, it is used for
		// getting resources only
		return null;
	}
}
