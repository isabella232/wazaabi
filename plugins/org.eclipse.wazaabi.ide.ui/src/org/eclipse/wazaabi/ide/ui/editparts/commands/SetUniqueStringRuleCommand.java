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

package org.eclipse.wazaabi.ide.ui.editparts.commands;

import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;

public class SetUniqueStringRuleCommand extends
		AbstractSetUniqueStyleRuleCommand {

	@Override
	protected StyleRule createNewStyleRule() {
		return CoreStylesFactory.eINSTANCE.createStringRule();
	}

	@Override
	protected void setStyleRuleValue(StyleRule rule, Object newValue) {
		if (rule instanceof StringRule)
			((StringRule) rule).setValue((String) newValue);
	}

	@Override
	protected Object getStyleRuleValue(StyleRule rule) {
		if (rule instanceof StringRule)
			return ((StringRule) rule).getValue();
		return null;
	}
}
