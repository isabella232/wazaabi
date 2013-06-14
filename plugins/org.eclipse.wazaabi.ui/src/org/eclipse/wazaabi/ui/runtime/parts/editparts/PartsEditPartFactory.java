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

package org.eclipse.wazaabi.ui.runtime.parts.editparts;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.ui.model.parts.PartsPackage;

public class PartsEditPartFactory implements EditPartFactory {

	public static final String FACTORY_ID = PartsEditPartFactory.class
			.getName();

	/**
	 * Maps an object to an EditPart.
	 * 
	 * @throws RuntimeException
	 *             if no match was found (programming error)
	 */
	private EditPart getPartForElement(Object modelElement) {
		if (modelElement instanceof EObject) {
			EClass eClass = ((EObject) modelElement).eClass();
			if (eClass == PartsPackage.Literals.PAGE)
				return new ContainerEditPart();
		}
		return null;
	}

	public String getId() {
		return FACTORY_ID;
	}

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		EditPart part = getPartForElement(model);
		if (part == null)
			return null;
		// store model element in EditPart
		part.setModel(model);
		return part;
	}

	@Override
	public boolean isFactoryFor(Object callingContext, Object model) {
		if (model instanceof EObject) {
			return PartsPackage.eINSTANCE.equals(((EObject) model).eClass()
					.getEPackage());
		}
		return false;
	}

	@Override
	public String getFactoryID() {
		return FACTORY_ID;
	}

}