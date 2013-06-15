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

	public static final String EDITPART_FACTORY_ID = "org.eclipse.wazaabi.engine.swt.commons.editparts.WazaabiEditPartFactory"; // $NON-NLs-1$

	private EditPart getPartForElement(Object modelElement) {
		return null;
	}

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		// get EditPart for model element
		EditPart part = getPartForElement(model);
		if (part == null)
			return null;
		// store model element in EditPart
		part.setModel(model);
		return part;
	}

	@Override
	public boolean isFactoryFor(Object callingContext, Object model, Object creationHint) {
		// return always false since SWTComponent.ecore does not own any Widget
		// definition
		return false;
	}

	@Override
	public String getFactoryID() {
		return EDITPART_FACTORY_ID;
	}

}