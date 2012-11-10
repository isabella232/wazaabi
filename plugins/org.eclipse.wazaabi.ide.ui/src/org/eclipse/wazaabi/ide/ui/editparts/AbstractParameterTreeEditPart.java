/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public abstract class AbstractParameterTreeEditPart extends
		AbstractTreeEditPart {

	@Override
	protected Image getImage() {
		return Activator.getDefault().getImageRegistry()
				.get(((EObject) getModel()).eClass().getName());
	}


}
