/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.table.editinghelpers.complexcelleditors.details;

public class UIContentsDescriptorFactory {

	private static final AbstractUIContentsDescriptor contents[] = new AbstractUIContentsDescriptor[] {};

	public AbstractUIContentsDescriptor getUIContentsDescriptor(
			Object descriptor) {
		for (AbstractUIContentsDescriptor detailsContent : contents)
			if (detailsContent.getUniqueID().equals(descriptor))
				return detailsContent;
		return null; // TODO : BLANK_DETAILS_CONTENT?
	}
}
