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

import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;

public interface ComponentFactory extends IdentifiedFactory {

	/**
	 * Creates and returns a component (which can be of any type of
	 * </code>Object</code>).
	 * 
	 * @param callingContext
	 *            The instance which calls this method
	 * @param model
	 *            the Object used as a model during the creation phase of the
	 *            component. For instance, {@link AbstractCodeDescriptor} are
	 *            created given a specific URI which is a {@link String}. In
	 *            this case, the model is the URI.
	 * @param creationHint
	 *            Any type of Object (may be null) which can help during the
	 *            creation phase of the component.
	 * @return A Object if the component has been created, null otherwise.
	 */
	public Object createComponent(Object callingContext, Object model,
			Object creationHint);

}
