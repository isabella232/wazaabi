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

package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors;

import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.AbstractUIContentsDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.UIContentsDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.layouts.FillLayoutForm;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.layouts.GridDataForm;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.layouts.GridLayoutForm;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.layouts.RowDataForm;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.layouts.RowLayoutForm;

public class FormBasedUIContentsDescriptorFactory extends
		UIContentsDescriptorFactory {

	private static final AbstractUIContentsDescriptor _contents[] = new AbstractUIContentsDescriptor[] {
			new RowLayoutForm(), new GridLayoutForm(), new FillLayoutForm(),
			new RowDataForm(), new GridDataForm() };

	public AbstractUIContentsDescriptor getUIContentsDescriptor(
			Object descriptor) {
		for (AbstractUIContentsDescriptor detailsContent : _contents)
			if (detailsContent.getUniqueID().equals(descriptor))
				return detailsContent;
		return super.getUIContentsDescriptor(descriptor); // TODO :
															// BLANK_DETAILS_CONTENT?
	}
}
