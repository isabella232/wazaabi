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

package org.eclipse.wazaabi.engine.swt.commons.editparts;

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;

public class SWTEditPartFactory implements EditPartFactory {


	private static final String EDITPART_FACTORY_ID = "org.eclipse.wazaabi.engine.swt.commons.editparts.WazaabiEditPartFactory"; // $NON-NLs-1$

	public EditPart createEditPart(EditPart context, Object modelElement) {
		// get EditPart for model element
		EditPart part = getPartForElement(modelElement);
		if (part == null)
			return null;
		// store model element in EditPart
		part.setModel(modelElement);
		return part;
	}

	/**
	 * Maps an object to an EditPart.
	 * 
	 * @throws RuntimeException
	 *             if no match was found (programming error)
	 */
	private EditPart getPartForElement(Object modelElement) {

		return null;
	}

	public String getId() {
		return EDITPART_FACTORY_ID;
	}

}