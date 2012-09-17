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
		if (object != null && object.eResource() != null
				&& object.eResource().getResourceSet() != null)
			return TransactionalEditingDomain.Factory.INSTANCE
					.getEditingDomain(object.eResource().getResourceSet());
		return null;
	}
}
