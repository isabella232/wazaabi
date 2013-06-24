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

package org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class FormLayoutStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(FormLayoutRule.class)) {
		case SWTStylesPackage.FORM_LAYOUT_RULE__MARGIN_BOTTOM:
		case SWTStylesPackage.FORM_LAYOUT_RULE__MARGIN_HEIGHT:
		case SWTStylesPackage.FORM_LAYOUT_RULE__MARGIN_LEFT:
		case SWTStylesPackage.FORM_LAYOUT_RULE__MARGIN_RIGHT:
		case SWTStylesPackage.FORM_LAYOUT_RULE__MARGIN_TOP:
		case SWTStylesPackage.FORM_LAYOUT_RULE__MARGIN_WIDTH:
		case SWTStylesPackage.FORM_LAYOUT_RULE__SPACING:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			break;
		default:
			super.notifyChanged(notification);
		}
		if (hasChanged
				&& getHost().styleRuleUpdated(
						(StyleRule) notification.getNotifier()))
			reCreateWidgetView();
	}

	private static FormLayout convertIntoSWTFormLayout(FormLayoutRule rule) {
		FormLayout formLayout = new FormLayout();
		formLayout.marginBottom = rule.getMarginBottom();
		formLayout.marginHeight = rule.getMarginHeight();
		formLayout.marginLeft = rule.getMarginLeft();
		formLayout.marginRight = rule.getMarginRight();
		formLayout.marginTop = rule.getMarginTop();
		formLayout.marginWidth = rule.getMarginWidth();
		formLayout.spacing = rule.getSpacing();
		return formLayout;
	}

	/**
	 * Synchronizes this Composite's layout with the given FormLayoutRule's
	 * data.
	 * 
	 */
	public static void platformSpecificRefresh(Object containerView,
			FormLayoutRule rule) {
		assert rule != null;
		if (!(containerView instanceof SWTContainerView))
			return;
		final Composite context = (Composite) ((SWTContainerView) containerView)
				.getContentPane();
		if (context == null || context.isDisposed())
			return;

		context.setLayout(convertIntoSWTFormLayout(rule));
	}
}
