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

package org.eclipse.wazaabi.locator.urn.java.codelocators;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeLocator;
import org.eclipse.wazaabi.locator.urn.java.codedescriptors.JavaCodeDescriptor;

public class UrnJavaCodeLocator extends AbstractCodeLocator {

	static private final String URI_PREFIX = "urn:java:"; //$NON-NLS-1$ 
	static private final int URI_PREFIX_LENGTH = URI_PREFIX.length();

	public static final String FACTORY_ID = UrnJavaCodeLocator.class.getName();

//	@Override
//	public AbstractCodeDescriptor resolveCodeDescriptor(String uri) {
//		String path = uri.substring(URI_PREFIX_LENGTH);
//		if (path != null && !"".equals(path)) //$NON-NLS-1$
//			return new JavaCodeDescriptor(path);
//		return null;
//	}

	public InputStream getResourceInputStream(String uri) throws IOException {
		final String path = uri.substring(URI_PREFIX_LENGTH);
		URL url = getClass().getClassLoader().getResource(path);
		if (url != null) {
			return url.openStream();
		}
		return null;
	}

	@Override
	public boolean isFactoryFor(Object callingContext, Object model, Object creationHint) {
		if (model instanceof String && ((String) model).startsWith(URI_PREFIX))
			return true;
		return false;
	}

	public String getFullPath(String prefix, String relativePath, Object context) {
		if (relativePath != null && relativePath.startsWith(URI_PREFIX))
			return relativePath;
		if (URI_PREFIX.equals(prefix))
			return URI_PREFIX + relativePath;
		return null;
	}

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		if (model instanceof String) {
			String path = ((String) model).substring(URI_PREFIX_LENGTH);
			if (path != null && !"".equals(path)) //$NON-NLS-1$
				return new JavaCodeDescriptor(path);
		}
		return null;
	}

	@Override
	public String getFactoryID() {
		return FACTORY_ID;
	}

}
