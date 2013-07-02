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

package org.eclipse.wazaabi.engine.swt.forms.editparts;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.wazaabi.engine.swt.forms.views.SWTLabelView;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class LabelEditPart extends
		org.eclipse.wazaabi.engine.core.editparts.LabelEditPart {

	public static final String RENDER_XML = "render-xml"; //$NON-NLS-1$
	public static final String KEY = "key"; //$NON-NLS-1$
	public static final String _KEY_PREFIX = KEY + ":"; //$NON-NLS-1$
	public static final int _KEY_PREFIX_LENGHT = _KEY_PREFIX.length();

	protected void refreshFeaturesAndStyles() {
		super.refreshFeaturesAndStyles();
		refreshUniqueStyleRule(RENDER_XML);
		refreshXMLStyles();
	}

	protected void refreshXMLStyles() {
		if (!(getWidgetView() instanceof SWTLabelView))
			return;
		HashMap<String, List<StyleRule>> result = new HashMap<String, List<StyleRule>>();
		for (StyleRule rule : ((StyledElement) getModel()).getStyleRules())
			if (rule instanceof ColorRule || rule instanceof FontRule
					|| rule instanceof ImageRule) {
				if (rule.getPropertyName() != null
						&& rule.getPropertyName().length() > _KEY_PREFIX_LENGHT
						&& rule.getPropertyName().startsWith(_KEY_PREFIX)) {
					String key = rule.getPropertyName().substring(
							_KEY_PREFIX_LENGHT);
					List<StyleRule> existingRules = result.get(key);
					if (existingRules == null) {
						existingRules = new ArrayList<StyleRule>();
						existingRules.add(rule);
						result.put(key, existingRules);
					} else {
						int idx = -1;
						for (int i = 0; i < existingRules.size(); i++) {
							StyleRule existingRule = existingRules.get(i);
							if (areSwitchable(existingRule, rule,
									ColorRule.class)
									|| areSwitchable(existingRule, rule,
											FontRule.class)
									|| areSwitchable(existingRule, rule,
											ImageRule.class)) {
								idx = i;
								break;
							}
						}
						if (idx != -1)
							existingRules.remove(idx);
						existingRules.add(rule);
					}
				}
			}
		if (!result.isEmpty())
			((SWTLabelView) getWidgetView()).updateXMLStyles(result);

	}

	/**
	 * Returns true if both rules inherits from clazz.
	 * 
	 * @param rule1
	 * @param rule2
	 * @param clazz
	 * @return
	 */
	private boolean areSwitchable(StyleRule rule1, StyleRule rule2,
			Class<?> clazz) {
		return ((clazz.isAssignableFrom(rule1.getClass()) && clazz
				.isAssignableFrom(rule2.getClass())));
	}
}
