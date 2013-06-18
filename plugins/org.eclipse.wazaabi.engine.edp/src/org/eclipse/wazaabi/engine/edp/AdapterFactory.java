/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
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

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;

public interface AdapterFactory extends IdentifiableFactory {

	/**
	 * Creates an Adapter for this given model and creation Hint
	 * 
	 * @param callingContext
	 *            The instance which calls this method
	 * @param model
	 *            The target of the adapter
	 * @param creationHint
	 *            Any Object (may be null) that could help during the creation
	 *            of the adapter.
	 * @return An <code>Adapter</code> or null if nothing can be created
	 * 
	 * @see Adapter
	 */
	public Adapter createAdapter(Object callingContext, EObject model,
			Object creationHint);

}
