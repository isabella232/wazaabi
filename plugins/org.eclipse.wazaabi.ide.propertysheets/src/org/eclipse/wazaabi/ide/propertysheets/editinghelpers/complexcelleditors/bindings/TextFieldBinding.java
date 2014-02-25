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

package org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;

public class TextFieldBinding extends AbstractBinding {

	@Override
	protected void addListeners(final Control control) {
		((Text) control).addModifyListener(new ModifyListener() {

			public void modifyText(ModifyEvent e) {
				String newStringValue = ((Text) control).getText();

				String errorMessage = getErrorMessage(newStringValue);
				Object domainValue = getDomainValue(control);

				if (errorMessage == null) {
					Object newValue = convertToExpectedValue(newStringValue);
					if (newValue == null) {
						if (domainValue == null)
							return;
					} else if (newValue.equals(domainValue))
						return;
					TargetChangeListener listener = getTargetChangeListener(control);
					if (listener != null)
						listener.targetModified(getDomainObject(control),
								getEStructuralFeature(control), -1,
								domainValue, newValue);
				} else
					; // TODO
			}
		});
	}

	@Override
	protected Object convertToExpectedValue(Object value) {
		return value;
	}

	@Override
	public void refresh(Control control) {
		((Text) control).setText((String) getDomainValue(control));
	}

}
