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

package org.eclipse.wazaabi.engine.swt.tests;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public class TestUtils {
/*
	public static Widget addRule(Widget widget, EClass ruleType, String type, String value) {
		StyleRule rule = (StyleRule) StylesFactory.eINSTANCE.create(ruleType);
		rule.setPropertyName(type);
		IntRule t;
		t.setValue(value);
		widget.getStyleRules().add(rule);
		return widget;
	}
*/
	
	public static Widget removeFirstRuleByRemove(Widget widget, String type) {
		for (int i = 0 ; i < widget.getStyleRules().size(); i++) {
			if (widget.getStyleRules().get(i).getPropertyName().equalsIgnoreCase(type)){
				widget.getStyleRules().remove(i);
				break;
			}
		}
		return widget;
	}
	
	public static Widget removeFirstRuleByRename(Widget widget, String type) {
		for (int i = 0 ; i < widget.getStyleRules().size(); i++) {
			if (widget.getStyleRules().get(i).getPropertyName().equalsIgnoreCase(type)){
				widget.getStyleRules().get(i).setPropertyName("");
				break;
			}
		}
		return widget;
	}
	
	public static Widget switchFirstAndSecondRule(Widget widget, String type) {
		int pos1 = -1, pos2 = -1;
		for (int i = 0 ; i < widget.getStyleRules().size(); i++) {
			if (widget.getStyleRules().get(i).getPropertyName().equalsIgnoreCase(type)){
				if (pos1 == -1) {
					pos1 = i;
					continue;
				}
				if (pos1 != -1 && pos2 == -1) {
					pos2 = i;
					break;
				}
			}
		}
		if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
			widget.getStyleRules().move(pos1,pos2);
		}
		return widget;
	}
	
	public static Widget renameRuleToNewRule(Widget widget, String oldPropertyName, String newPropertyName){
		StyleRule rule = widget.getFirstStyleRule(oldPropertyName, null);
		rule.setPropertyName(newPropertyName);
		return widget;
		
	}
	
	
}