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

package org.eclipse.wazaabi.ide.ui.editors;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.gef.requests.CreationFactory;

public class EMFPaletteCreationFactory implements CreationFactory {

	private EClass eClass;

	public EMFPaletteCreationFactory(EClass eClass) {
		this.eClass = eClass;
	}

	public Object getNewObject() {
		if (getEClass() != null)
			return getEClass().getEPackage().getEFactoryInstance()
					.create(getEClass());
		return null;
	}

	public Object getObjectType() {
		return getEClass();
	}

	public EClass getEClass() {
		return eClass;
	}

}
