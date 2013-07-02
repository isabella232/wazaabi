/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.forms.stylerules.factories;

import org.eclipse.wazaabi.engine.core.stylerules.factories.CoreStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.swt.forms.stylerules.managers.XMLColorStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.forms.stylerules.managers.XMLFontRuleManager;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

public class SWTFormsStyleRuleManagerFactory extends
		CoreStyleRuleManagerFactory {

	public static final String FACTORY_ID = SWTFormsStyleRuleManagerFactory.class
			.getName();

	public void platformSpecificRefresh(Object context, StyleRule rule) {
		// nothing to do here since by definition, the core bundle is not
		// platform specific
	}

	public void platformSpecificUpdate(Object context, StyleRule rule) {
		// nothing to do here since by definition, the core bundle is not
		// platform specific
	}

	public Object convertIntoPlatformSpecificObject(Object context,
			StyleRule rule) {
		// nothing to do and return here since by definition, the core bundle is
		// not platform specific
		return null;
	}

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		if (model instanceof StyleRule) {
			StyleRule rule = (StyleRule) model;
			if (rule != null
					&& CoreStylesPackage.eNS_URI.equals(rule.eClass()
							.getEPackage().getNsURI()))
				switch (rule.eClass().getClassifierID()) {

				case CoreStylesPackage.COLOR_RULE:
					return new XMLColorStyleRuleManager();

				case CoreStylesPackage.FONT_RULE:
					return new XMLFontRuleManager();
				default:
					return super.createComponent(callingContext, model,
							creationHint);
				}
		}
		return null;

	}

	@Override
	public String getFactoryID() {
		return FACTORY_ID;
	}
}
