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

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import org.eclipse.emf.ecore.EObject;

public class Transformer {

	public EObject getSubTree(EObject targetUI, EObject sourceModel, int index) {
		if (targetUI == null || sourceModel == null)
			return null;
		
		return null;
	}

}
