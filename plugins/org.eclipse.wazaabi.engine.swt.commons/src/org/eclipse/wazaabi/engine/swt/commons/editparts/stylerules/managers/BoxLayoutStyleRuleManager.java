/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises - initial API and implementation
 *   Pavel Erofeev - common BoxLayoutRule
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;

public class BoxLayoutStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(RowLayoutRule.class)) {
		case CoreStylesPackage.BOX_LAYOUT_RULE__MARGIN:
		case CoreStylesPackage.BOX_LAYOUT_RULE__SPACING:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			break;
		case CoreStylesPackage.BOX_LAYOUT_RULE__ORIENTATION:
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

	private static RowLayout convertIntoSWTRowLayout(BoxLayoutRule rule) {
		RowLayout rowLayout = new RowLayout();
		rowLayout.marginBottom = rule.getMargin();
		rowLayout.marginLeft = rule.getMargin();
		rowLayout.marginRight = rule.getMargin();
		rowLayout.marginTop = rule.getMargin();
		rowLayout.spacing = rule.getSpacing();
		rowLayout.wrap = false;
		if (rule.getOrientation() == Orientation.VERTICAL)
			rowLayout.type = SWT.VERTICAL;
		else
			rowLayout.type = SWT.HORIZONTAL;
		return rowLayout;
	}

	/**
	 * Synchronizes this Composite's layout with the given RowLayoutRule's data.
	 * Ensures that, at the end of the method execution, all children of the
	 * Composite have a layout's data either null or of type RowData.
	 * 
	 */
	public static void platformSpecificRefresh(Object containerView,
			BoxLayoutRule rule) {
		assert rule != null;
		if (!(containerView instanceof SWTContainerView))
			return;
		final Composite context = (Composite) ((SWTContainerView) containerView)
				.getContentPane();
		if (context == null || context.isDisposed())
			return;

		final Layout previousLayout = context.getLayout();
		context.setLayout(convertIntoSWTRowLayout(rule));

		// we check if we changed the type of the layout
		if (previousLayout == null && context.getLayout() == null)
			return;
		if (previousLayout != null
				&& context.getLayout() != null
				&& previousLayout.getClass().equals(
						context.getLayout().getClass()))
			return;

		for (Control child : context.getChildren())
			if (!(child.getLayoutData() instanceof RowData)) {
				Object data = child.getData(SWTWidgetView.WAZAABI_HOST_KEY);
				if (data instanceof AbstractComponentEditPart)
					child.setLayoutData(RowDataStyleRuleManager
							.getFirstRowData((AbstractComponentEditPart) data));
			}
	}
}
