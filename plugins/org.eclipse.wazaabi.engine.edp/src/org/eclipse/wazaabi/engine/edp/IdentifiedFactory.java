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

package org.eclipse.wazaabi.engine.edp;

public interface IdentifiedFactory {

	/**
	 * Returns true if this factory is a factory for this source in the given
	 * calling context. This method is used when selecting the appropriate
	 * factory during a lookup of registered ones.
	 * 
	 * @param callingContext
	 *            The instance of the caller
	 * @param model
	 *            the model argument for the creation of adapter or component.
	 * @param creationHint TODO
	 * @return true if the factory can be used for these given parameters
	 */
	public boolean isFactoryFor(Object callingContext, Object model, Object creationHint);

	/**
	 * Returns the ID of the factory
	 * 
	 * @return A non null String
	 */
	public String getFactoryID();

}
