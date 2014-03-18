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

package org.eclipse.wazaabi.ide.propertysheets.graphicalhelpers;

import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;

public class BindingGraphicalHelper extends EventHandlerGraphicalHelper {

	@Override
	protected String getOtherClause(EventHandler binding) {
		String result = "";
		StringParameter sourceParameter = null;
		StringParameter targetParameter = null;
		for (Parameter p : binding.getParameters())
			if (p instanceof StringParameter) {
				if ("source".equals(p.getName()))
					sourceParameter = (StringParameter) p;
				else if ("target".equals(p.getName()))
					targetParameter = (StringParameter) p;
				if (sourceParameter != null && targetParameter != null)
					break;
			}
		if (sourceParameter != null && sourceParameter.getValue() != null
				&& !sourceParameter.getValue().isEmpty())
			result += sourceParameter.getValue();
		result += "->";
		if (targetParameter != null && targetParameter.getValue() != null
				&& !targetParameter.getValue().isEmpty())
			result += targetParameter.getValue();
		if (result.equals("->"))
			return "";
		return result;
	}
}
