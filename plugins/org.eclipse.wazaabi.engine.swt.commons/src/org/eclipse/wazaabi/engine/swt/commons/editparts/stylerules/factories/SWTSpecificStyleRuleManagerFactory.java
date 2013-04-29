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

package org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.factories;

import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.core.stylerules.factories.StyleRuleManagerFactory;
import org.eclipse.wazaabi.engine.core.stylerules.managers.TabRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.FillLayoutStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.FormDataStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.FormLayoutStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.GridDataStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.GridLayoutStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.ImageRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.RowDataStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.RowLayoutStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.StackLayoutStyleRuleManager;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.TabbedLayoutRuleManager;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.FormDataRule;
import org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class SWTSpecificStyleRuleManagerFactory implements
		StyleRuleManagerFactory {

	public StyleRuleManager createStyleRuleManager(StyleRule rule) {
		if (rule == null)
			return null;
		if (SWTStylesPackage.eNS_URI.equals(rule.eClass().getEPackage()
				.getNsURI()))
			switch (rule.eClass().getClassifierID()) {
			case SWTStylesPackage.ROW_DATA_RULE:
				return new RowDataStyleRuleManager();
			case SWTStylesPackage.ROW_LAYOUT_RULE:
				return new RowLayoutStyleRuleManager();
			case SWTStylesPackage.GRID_LAYOUT_RULE:
				return new GridLayoutStyleRuleManager();
			case SWTStylesPackage.GRID_DATA_RULE:
				return new GridDataStyleRuleManager();
			case SWTStylesPackage.FILL_LAYOUT_RULE:
				return new FillLayoutStyleRuleManager();
			case SWTStylesPackage.FORM_LAYOUT_RULE:
				return new FillLayoutStyleRuleManager();
			case SWTStylesPackage.FORM_DATA_RULE:
				return new FormDataStyleRuleManager();
			}
		else if (CoreStylesPackage.eNS_URI.equals(rule.eClass().getEPackage()
				.getNsURI()))
			switch (rule.eClass().getClassifierID()) {
			case CoreStylesPackage.IMAGE_RULE:
				return new ImageRuleManager();
			case CoreStylesPackage.STACK_LAYOUT_RULE:
				return new StackLayoutStyleRuleManager();
			case CoreStylesPackage.TABBED_LAYOUT_RULE:
				return new TabbedLayoutRuleManager();
			case CoreStylesPackage.TAB_RULE:
				return new TabRuleManager();			}

		return null;
	}

	public boolean isFactoryFor(StyleRule rule) {
		if (rule == null)
			return false;
		if (SWTStylesPackage.eNS_URI.equals(rule.eClass().getEPackage()
				.getNsURI()))
			switch (rule.eClass().getClassifierID()) {
			case SWTStylesPackage.ROW_DATA_RULE:
			case SWTStylesPackage.ROW_LAYOUT_RULE:
			case SWTStylesPackage.GRID_LAYOUT_RULE:
			case SWTStylesPackage.GRID_DATA_RULE:
			case SWTStylesPackage.FILL_LAYOUT_RULE:
			case SWTStylesPackage.FORM_LAYOUT_RULE:
			case SWTStylesPackage.FORM_DATA_RULE:
				return true;
			}
		else if (CoreStylesPackage.eNS_URI.equals(rule.eClass().getEPackage()
				.getNsURI()))
			switch (rule.eClass().getClassifierID()) {
			case CoreStylesPackage.IMAGE_RULE:
			case CoreStylesPackage.STACK_LAYOUT_RULE:
			case CoreStylesPackage.TABBED_LAYOUT_RULE:
			case CoreStylesPackage.TAB_RULE:
				return true;
			}

		return false;
	}

	public void platformSpecificRefresh(Object context, StyleRule rule) {
		if (rule instanceof RowDataRule)
			RowDataStyleRuleManager.platformSpecificRefresh(context,
					(RowDataRule) rule);
		else if (rule instanceof RowLayoutRule)
			RowLayoutStyleRuleManager.platformSpecificRefresh(context,
					(RowLayoutRule) rule);
		else if (rule instanceof StackLayoutRule)
			StackLayoutStyleRuleManager.platformSpecificRefresh(context,
					(StackLayoutRule) rule);
		else if (rule instanceof GridLayoutRule)
			GridLayoutStyleRuleManager.platformSpecificRefresh(context,
					(GridLayoutRule) rule);
		else if (rule instanceof GridDataRule)
			GridDataStyleRuleManager.platformSpecificRefresh(context,
					(GridDataRule) rule);
		else if (rule instanceof FillLayoutRule)
			FillLayoutStyleRuleManager.platformSpecificRefresh(context,
					(FillLayoutRule) rule);
		else if (rule instanceof FormLayoutRule)
			FormLayoutStyleRuleManager.platformSpecificRefresh(context,
					(FormLayoutRule) rule);
		else if (rule instanceof FormDataRule)
			FormDataStyleRuleManager.platformSpecificRefresh(context,
					(FormDataRule) rule);
	}

	public void platformSpecificUpdate(Object context, StyleRule rule) {
		if (rule instanceof StackLayoutRule)
			StackLayoutStyleRuleManager.platformSpecificUpdate(context,
					(StackLayoutRule) rule);
	}

	public Object convertIntoPlatformSpecificObject(Object context,
			StyleRule rule) {
		if (rule instanceof ImageRule)
			return ImageRuleManager.convertToPlatformSpecificObject(context,
					(ImageRule) rule);
		return null;
	}
}
