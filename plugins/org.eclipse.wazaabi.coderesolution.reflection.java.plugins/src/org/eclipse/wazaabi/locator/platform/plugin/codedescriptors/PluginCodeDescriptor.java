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

package org.eclipse.wazaabi.coderesolution.reflection.java.plugins.codedescriptors;

import org.eclipse.wazaabi.coderesolution.reflection.java.plugins.Activator;
import org.eclipse.wazaabi.locator.urn.java.codedescriptors.JavaCodeDescriptor;
import org.osgi.framework.Bundle;

public class PluginCodeDescriptor extends JavaCodeDescriptor {

	private final String bundleSymbolicName;

	// do not change this field name since it is accessed during tests
	private Bundle resolvedBundle = null;

	protected Bundle getResolvedBundle() {
		return resolvedBundle;
	}

	public PluginCodeDescriptor(String bundleSymbolicName, String javaClassName) {
		super(javaClassName);
		this.bundleSymbolicName = bundleSymbolicName;
		if (Activator.getDefault() != null)
			this.resolvedBundle = Activator.getDefault().getBundleForName(
					bundleSymbolicName);
	}

	public String getBundleSymbolicName() {
		return bundleSymbolicName;
	}

	@Override
	protected Class<?> resolveClass() {
		if (getResolvedBundle() != null) {
			try {
				return getResolvedBundle().loadClass(getJavaClassName());
			} catch (ClassNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return super.resolveClass();
	}

}
