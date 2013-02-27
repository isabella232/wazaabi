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

package org.eclipse.wazaabi.ide.ui.editparts.commands;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.transaction.TransactionalEditingDomain;

public class CommandsUtils {

	/**
	 * Returns the TransactionalEditingDomain this object belongs to.
	 * 
	 * @param object
	 *            A EObject
	 * @return A TransactionalEditingDomain if found, null otherwise.
	 */
	public static final TransactionalEditingDomain getEditingDomain(
			EObject object) {
		if (object != null)
			return getEditingDomain(object.eResource());
		return null;
	}

	/**
	 * Returns the TransactionalEditingDomain this resource belongs to.
	 * 
	 * @param resource
	 *            A Resource
	 * @return A TransactionalEditingDomain if found, null otherwise.
	 */
	public static final TransactionalEditingDomain getEditingDomain(
			Resource resource) {
		if (resource != null && resource.getResourceSet() != null)
			return TransactionalEditingDomain.Factory.INSTANCE
					.getEditingDomain(resource.getResourceSet());
		return null;
	}
}
