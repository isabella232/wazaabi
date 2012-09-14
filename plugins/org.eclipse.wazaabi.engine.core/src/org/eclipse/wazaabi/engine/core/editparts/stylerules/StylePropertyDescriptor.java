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

package org.eclipse.wazaabi.engine.core.editparts.stylerules;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl;

public class StylePropertyDescriptor {

	private String name = null;
	private EClass type = null;
	private StyleRule defaultStyleRule = null;

	public StylePropertyDescriptor(String name, EClass type,
			StyleRule defaultStyleRule) {
		this.name = name;
		this.type = type;
		if (defaultStyleRule != null) {
			this.defaultStyleRule = defaultStyleRule;
			defaultStyleRule.setPropertyName(name);
		} else
			this.defaultStyleRule = new BlankRuleImpl() {
				@Override
				public String getPropertyName() {
					return StylePropertyDescriptor.this.getName();
				}
			};
	}

	public String getName() {
		return name;
	}

	public EClass getType() {
		return type;
	}

	public StyleRule getDefaultStyleRule() {
		return defaultStyleRule;
	}

};
