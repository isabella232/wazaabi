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

package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;

public class AbstractButtonTreeEditPart extends AbstractComponentTreeEditPart {

	protected String getTextRuleValue() {
		for (StyleRule rule : ((AbstractComponent) getModel()).getStyleRules())
			if ("text".equals(rule.getPropertyName()) //$NON-NLS-1$
					&& rule instanceof StringRule)
				return ((StringRule) rule).getValue();
		return null;
	}

	@Override
	protected String getLabel() {
		String textRuleValue = getTextRuleValue();
		if (textRuleValue != null)
			return super.getLabel() + " [" + textRuleValue + "]";
		return super.getLabel();
	}

	@Override
	public void styleRuleAdded(StyleRule newRule) {
		if (newRule != null && "text".equals(newRule.getPropertyName())) //$NON-NLS-1$
			refreshVisuals();
		super.styleRuleAdded(newRule);
	}

	@Override
	public void styleRuleRemoved(StyleRule oldRule) {
		if (oldRule != null && "text".equals(oldRule.getPropertyName())) //$NON-NLS-1$
			refreshVisuals();
		super.styleRuleRemoved(oldRule);
	}

	@Override
	public void styleRuleUpdated(StyleRule rule) {
		if (rule != null && "text".equals(rule.getPropertyName())) //$NON-NLS-1$
			refreshVisuals();
		super.styleRuleUpdated(rule);
	}

}
