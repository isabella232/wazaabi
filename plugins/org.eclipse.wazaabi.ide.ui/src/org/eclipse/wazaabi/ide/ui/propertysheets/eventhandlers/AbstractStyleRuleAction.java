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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public abstract class AbstractStyleRuleAction extends AbstractAction {


	protected Object getFirstStyleRuleValue(Widget widget, String propertyName) {
		if (propertyName == null || "".equals(propertyName)) //$NON-NLS-1$
			return NO_STYLE_RULE;
		for (StyleRule rule : widget.getStyleRules())
			if (propertyName.equals(rule.getPropertyName())
					&& rule instanceof StringRule)
				return ((StringRule) rule).getValue();
		return NO_STYLE_RULE;
	}



	/**
	 * Returns a dispatcher's EAttribute defined by its path.
	 * 
	 * @param dispatcher
	 * @param eventHandler
	 * @return An EAttribute when found, null otherwise.
	 */
	protected EAttribute resolveEAttribute(AbstractComponent dispatcher,
			EventHandler eventHandler, String path) {
		if (path == null || "".equals(path)) //$NON-NLS-1$
			return null;
		if (path.startsWith("@")) { //$NON-NLS-1$
			path = path.substring(1);
			EStructuralFeature feature = dispatcher.eClass()
					.getEStructuralFeature(path);
			if (feature instanceof EAttribute)
				return ((EAttribute) feature);
		}
		return null;
	}

	protected abstract boolean areEquals(Object uiValue, Object domainValue);

}
