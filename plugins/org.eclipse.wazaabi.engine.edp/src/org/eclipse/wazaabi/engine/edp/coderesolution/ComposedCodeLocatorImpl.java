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

package org.eclipse.wazaabi.engine.edp.coderesolution;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

public class ComposedCodeLocatorImpl implements ComposedCodeLocator {

	private List<ICodeLocator> codeLocators = new ArrayList<ICodeLocator>();

	public void addCodeLocator(ICodeLocator codeLocator) {
		codeLocators.add(codeLocator);
	}

	public void removeCodeLocator(ICodeLocator codeLocator) {
		codeLocators.remove(codeLocator);
	}

	public AbstractCodeDescriptor resolveCodeDescriptor(String uri) {
		for (ICodeLocator codeLocator : codeLocators)
			if (codeLocator.isCodeLocatorFor(uri))
				return codeLocator.resolveCodeDescriptor(uri);
		return null;
	}

	public boolean isCodeLocatorFor(String uri) {
		for (ICodeLocator codeLocator : codeLocators)
			if (codeLocator.isCodeLocatorFor(uri))
				return true;
		return false;
	}

	public InputStream getResourceInputStream(String uri) throws IOException {
		for (ICodeLocator codeLocator : codeLocators)
			if (codeLocator.isCodeLocatorFor(uri))
				return codeLocator.getResourceInputStream(uri);
		return null;
	}

	public String getFullPath(String prefix, String relativePath, Object context) {
		for (ICodeLocator codeLocator : codeLocators) {
			String uri = codeLocator.getFullPath(prefix, relativePath, context);
			if (uri != null)
				return uri;
		}
		return null;
	}

}
