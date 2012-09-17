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

public class UpdateStringRuleAction extends UpdateStyleRuleAction {

	protected boolean areEquals(Object uiValue, Object domainValue) {
		if ((uiValue == null || "".equals(uiValue)) && (domainValue == null || AbstractStyleRuleAction.NO_STYLE_RULE == domainValue)) //$NON-NLS-1$
			return true;
		if (uiValue != null && uiValue.equals(domainValue))
			return true;
		if (domainValue != null && domainValue.equals(uiValue))
			return true;
		return false;
	}

}
