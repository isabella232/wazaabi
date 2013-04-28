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

package org.eclipse.wazaabi.engine.edp.internal;

import java.util.HashMap;

import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;

public class RegistryImpl implements Registry {

	private HashMap<String, IPointersEvaluator> pointersEvaluators = new HashMap<String, IPointersEvaluator>();

	private static final String DEFAULT_POINTERS_PROVIDER_ID = "org.eclipse.wazaabi.locationpaths.PointersEvaluatorImpl"; //$NON-NLS-1$

	public void addPointersEvaluator(IPointersEvaluator pointersEvaluator) {
		if (pointersEvaluator != null)
			pointersEvaluators.put(pointersEvaluator.getClass().getName(),
					pointersEvaluator);
	}

	public void removePointersEvaluator(IPointersEvaluator pointersEvaluator) {
		if (pointersEvaluator != null)
			pointersEvaluators.remove(pointersEvaluator.getClass().getName());
	}

	public synchronized IPointersEvaluator getPointersEvaluator(String id) {
		return pointersEvaluators.get(id);
	}

	// TODO : temporary, until found a better way to do this
	public synchronized IPointersEvaluator getDefaultPointersEvaluator() {
		if (pointersEvaluators.size() == 1)
			return pointersEvaluators.values().toArray(
					new IPointersEvaluator[0])[0];
		return pointersEvaluators.get(DEFAULT_POINTERS_PROVIDER_ID);
	}

//	private String codeLocatorPrefixes[] = null;
//	private ICodeLocator codeLocators[] = null;
//
//	public void addCodeLocator(ICodeLocator codeLocator) {
//		if (codeLocator != null)
//			registeredCodeLocators.add(codeLocator);
//		resetCodeLocators();
//	}
//
//	public void removeCodeLocator(ICodeLocator codeLocator) {
//		if (codeLocator != null)
//			registeredCodeLocators.remove(codeLocator);
//		resetCodeLocators();
//	}
//
//	private void resetCodeLocators() {
//		codeLocators = registeredCodeLocators.toArray(new ICodeLocator[0]);
//		codeLocatorPrefixes = new String[codeLocators.length];
//		for (int i = 0; i < codeLocators.length; i++)
//			codeLocatorPrefixes[i] = codeLocators[i].getCodeLocatorPrefix();
//	}
//
//	/**
//	 * Returns the <code>AbstractCodeLocator</code> corresponding to the given
//	 * uri's prefix.
//	 * 
//	 * @param uri
//	 *            the uri used to resolve the <code>AbstractCodeLocator</code>
//	 * @return the <code>AbstractCodeLocator</code> whose prefix matches the
//	 *         uri's prefix, null otherwise.
//	 */
//	public ICodeLocator resolveCodeLocator(String uri) {
//		if (codeLocators == null)
//			return null;
//		if (uri == null || "".equals(uri)) //$NON-NLS-1$
//			return null;
//		for (int i = 0; i < codeLocators.length; i++)
//			if (uri.startsWith(codeLocatorPrefixes[i]))
//				return codeLocators[i];
//		return null;
//	}
}
