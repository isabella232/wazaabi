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
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

public class StackLayoutStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(StackLayoutRule.class)) {
		case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_HEIGHT:
		case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_WIDTH:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			break;
		case CoreStylesPackage.STACK_LAYOUT_RULE__TOP:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			// the topComponent could not have changed while the topControl has
			// changed
			if (!hasChanged
					&& getHost().getWidgetView() instanceof SWTContainerView) {
				Composite composite = (Composite) ((SWTContainerView) getHost()
						.getWidgetView()).getSWTWidget();
				if (composite.getLayout() instanceof StackLayout
						&& ((StackLayout) composite.getLayout()).topControl != getTopComponent(
								composite, notification.getNewIntValue()))
					hasChanged = true;
			}
			break;
		default:
			super.notifyChanged(notification);
		}
		if (hasChanged
				&& getHost().styleRuleUpdated(
						(StyleRule) notification.getNotifier()))
			reCreateWidgetView();
	}

	public static StackLayout convertIntoSWTStackLayout(StackLayoutRule rule) {
		StackLayout stackLayout = new StackLayout();
		stackLayout.marginHeight = rule.getMarginHeight();
		stackLayout.marginWidth = rule.getMarginWidth();
		// TODO : we need to set 'top' here
		return stackLayout;
	}

	public static void platformSpecificRefresh(Object containerView,
			StackLayoutRule rule) {
		assert rule != null;
		if (!(containerView instanceof SWTContainerView))
			return;
		final Composite context = (Composite) ((SWTContainerView) containerView)
				.getSWTWidget();
		if (context == null || context.isDisposed())
			return;

		context.setLayout(convertIntoSWTStackLayout(rule));
		refreshTopValue(context, rule.getTop());
	}

	public static void platformSpecificUpdate(Object containerView,
			StackLayoutRule rule) {
		assert rule != null;
		if (!(containerView instanceof SWTContainerView))
			return;
		final Composite context = (Composite) ((SWTContainerView) containerView)
				.getSWTWidget();
		if (!(context.getLayout() instanceof StackLayout))
			return;
		if (context == null || context.isDisposed())
			return;

		Control top = getTopComponent(context, rule.getTop());
		if (((StackLayout) context.getLayout()).topControl == top)
			return;
		((StackLayout) context.getLayout()).topControl = top;

	}

	protected static void refreshTopValue(final Composite composite,
			final int topValue) {
		Control top = getTopComponent(composite, topValue);

		if (composite.getLayout() instanceof StackLayout)
			((StackLayout) composite.getLayout()).topControl = top;

	}

	protected static Control getTopComponent(final Composite composite,
			final int idx) {
		// TODO : we need to check if the child is an AbstractComponent
		// possible case : a non AbstractComponent's control inserted among
		// others.
		if (idx >= 0 && idx < composite.getChildren().length)
			return composite.getChildren()[idx];
		return null;
	}
}
