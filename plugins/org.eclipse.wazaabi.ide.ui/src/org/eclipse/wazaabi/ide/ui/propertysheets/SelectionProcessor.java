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

package org.eclipse.wazaabi.ide.ui.propertysheets;

import org.eclipse.wazaabi.ide.ui.editparts.AbstractTreeEditPart;

public class SelectionProcessor {

	public Object getObject(Object object) {
		if (object instanceof AbstractTreeEditPart)
			return ((AbstractTreeEditPart) object).getModel();
		return object;
	}
}
