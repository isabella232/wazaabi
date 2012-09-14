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

package org.eclipse.wazaabi.engine.swt.editparts.stylerules.managers;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.views.SWTContainerView;

public class FillLayoutStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(FillLayoutRule.class)) {
		case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_HEIGHT:
		case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_WIDTH:
		case SWTStylesPackage.FILL_LAYOUT_RULE__SPACING:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			break;
		case SWTStylesPackage.FILL_LAYOUT_RULE__TYPE:
			hasChanged = !org.eclipse.wazaabi.engine.core.CompareUtils
					.areEquals((Orientation) notification.getOldValue(),
							(Orientation) notification.getNewValue());
			break;
		default:
			super.notifyChanged(notification);
		}
		if (hasChanged
				&& getHost().styleRuleUpdated(
						(StyleRule) notification.getNotifier()))
			reCreateWidgetView();
	}

	private static FillLayout convertIntoSWTFillLayout(FillLayoutRule rule) {
		FillLayout fillLayout = new FillLayout();
		fillLayout.marginHeight = rule.getMarginHeight();
		fillLayout.marginWidth = rule.getMarginWidth();
		fillLayout.spacing = rule.getSpacing();
		if (rule.getType() == Orientation.VERTICAL)
			fillLayout.type = SWT.VERTICAL;
		else
			fillLayout.type = SWT.HORIZONTAL;
		return fillLayout;
	}

	/**
	 * Synchronizes this Composite's layout with the given FillLayoutRule's
	 * data.
	 * 
	 */
	public static void platformSpecificRefresh(Object containerView,
			FillLayoutRule rule) {
		assert rule != null;
		if (!(containerView instanceof SWTContainerView))
			return;
		final Composite context = (Composite) ((SWTContainerView) containerView)
				.getSWTWidget();
		if (context == null || context.isDisposed())
			return;

		context.setLayout(convertIntoSWTFillLayout(rule));
	}
}
