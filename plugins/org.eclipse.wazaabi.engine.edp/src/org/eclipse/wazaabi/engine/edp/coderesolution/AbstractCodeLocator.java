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

public abstract class AbstractCodeLocator implements ICodeLocator {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.wazaabi.engine.core.adapter.runtime.ICodeLocator#
	 * resolveCodeDescriptor(java.lang.String)
	 */
	public abstract AbstractCodeDescriptor resolveCodeDescriptor(String uri);

	public abstract boolean isCodeLocatorFor(String uri);

}
