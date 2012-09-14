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

package org.eclipse.wazaabi.engine.core.stylerules.factories.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.core.stylerules.factories.ComposedStyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.stylerules.factories.StyleRuleManagerFactory;

public class ComposedStyleRuleManagerFactoryImpl implements
		ComposedStyleRuleManagerFactory {

	private List<StyleRuleManagerFactory> factories = new ArrayList<StyleRuleManagerFactory>();

	public StyleRuleManager createStyleRuleManager(StyleRule rule) {
		for (StyleRuleManagerFactory factory : factories)
			if (factory.isFactoryFor(rule)) {
				StyleRuleManager manager = factory.createStyleRuleManager(rule);
				if (manager != null)
					return manager;
			}

		return null;
	}

	public void addStyleRuleManagerFactory(StyleRuleManagerFactory factory) {
		if (!factories.contains(factory)) {
//			System.out.println("(style) adding " + factory);
			factories.add(factory);
		}
	}

	public void removeStyleRuleManagerFactory(StyleRuleManagerFactory factory) {
		factories.remove(factory);
//		System.out.println("(style) removing " + factory);
	}

	public boolean isFactoryFor(StyleRule rule) {
		for (StyleRuleManagerFactory factory : factories)
			if (factory.isFactoryFor(rule))
				return true;
		return false;
	}

	public void platformSpecificRefresh(Object context, StyleRule rule) {
		for (StyleRuleManagerFactory factory : factories)
			if (factory.isFactoryFor(rule)) {
				factory.platformSpecificRefresh(context, rule);
				break; // TODO : do we need to break the loop ?
			}
	}

	public void platformSpecificUpdate(Object context, StyleRule rule) {
		for (StyleRuleManagerFactory factory : factories)
			if (factory.isFactoryFor(rule)) {
				factory.platformSpecificUpdate(context, rule);
				break; // TODO : do we need to break the loop ?
			}
	}

	public Object convertIntoPlatformSpecificObject(Object context,
			StyleRule rule) {
		for (StyleRuleManagerFactory factory : factories)
			if (factory.isFactoryFor(rule))
				return factory.convertIntoPlatformSpecificObject(context, rule);
		return null;
	}

}
