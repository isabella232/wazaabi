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

package org.eclipse.wazaabi.ide.ui.propertysheets.eventhandlers;

import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public abstract class AbstractAction {

	public static final String EDIT_DOMAIN_KEY = "EditDomain"; //$NON-NLS-1$
	public static final String TRANSATIONAL_EDITING_DOMAIN_KEY = "TransactionalEditingDomain"; //$NON-NLS-1$
	public static final Object NO_STYLE_RULE = new Object() {
	};

	protected String getStringParameterValue(Parameterized parameterized,
			String parameterName) {
		if (parameterName == null || "".equals(parameterName))
			return null;
		for (Parameter parameter : parameterized.getParameters())
			if (parameterName.equals(parameter.getName())
					&& parameter instanceof StringParameter)
				return ((StringParameter) parameter).getValue();
		return null;
	}

	protected void updateStringParameterValue(Parameterized parameterized,
			String parameterName, String newValue) {
		if (parameterName == null || "".equals(parameterName))
			return;
		for (Parameter parameter : parameterized.getParameters())
			if (parameterName.equals(parameter.getName())
					&& parameter instanceof StringParameter)
				((StringParameter) parameter).setValue(newValue);
	}

	/**
	 * Returns a widget which is a context's entry whose key is 'input'. The
	 * context is the one attached to uiComponent.
	 * 
	 * @param uiComponent
	 *            The AbstractComponent whose context is looked up.
	 * @return A @link {@link Widget}, null otherwise.
	 */
	protected Widget getInputWidget(AbstractComponent uiComponent) {
		if (uiComponent.get("input") instanceof Widget) //$NON-NLS-1$
			return (Widget) uiComponent.get("input"); //$NON-NLS-1$
		return null;
	}

}
