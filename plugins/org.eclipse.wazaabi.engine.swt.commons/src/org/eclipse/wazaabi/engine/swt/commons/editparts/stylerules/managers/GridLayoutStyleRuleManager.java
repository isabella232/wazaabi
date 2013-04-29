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
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;

public class GridLayoutStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(GridLayoutRule.class)) {
		case SWTStylesPackage.GRID_LAYOUT_RULE__MAKE_COLUMNS_EQUAL_WIDTH:
			hasChanged = !CompareUtils.areEquals(
					notification.getOldBooleanValue(),
					notification.getNewBooleanValue());
			break;
		case SWTStylesPackage.GRID_LAYOUT_RULE__HORIZONTAL_SPACING:
		case SWTStylesPackage.GRID_LAYOUT_RULE__MARGIN_BOTTOM:
		case SWTStylesPackage.GRID_LAYOUT_RULE__MARGIN_HEIGHT:
		case SWTStylesPackage.GRID_LAYOUT_RULE__MARGIN_LEFT:
		case SWTStylesPackage.GRID_LAYOUT_RULE__MARGIN_RIGHT:
		case SWTStylesPackage.GRID_LAYOUT_RULE__MARGIN_TOP:
		case SWTStylesPackage.GRID_LAYOUT_RULE__MARGIN_WIDTH:
		case SWTStylesPackage.GRID_LAYOUT_RULE__NUM_COLUMNS:
		case SWTStylesPackage.GRID_LAYOUT_RULE__VERTICAL_SPACING:
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

	private static GridLayout convertIntoSWTGridLayout(GridLayoutRule rule) {
		GridLayout gridLayout = new GridLayout();
		gridLayout.horizontalSpacing = rule.getHorizontalSpacing();
		gridLayout.makeColumnsEqualWidth = rule.isMakeColumnsEqualWidth();
		gridLayout.marginBottom = rule.getMarginBottom();
		gridLayout.marginHeight = rule.getMarginHeight();
		gridLayout.marginLeft = rule.getMarginLeft();
		gridLayout.marginRight = rule.getMarginRight();
		gridLayout.marginTop = rule.getMarginTop();
		gridLayout.marginWidth = rule.getMarginWidth();
		gridLayout.numColumns = rule.getNumColumns();
		gridLayout.verticalSpacing = rule.getVerticalSpacing();
		return gridLayout;
	}

	/**
	 * Synchronizes this Composite's layout with the given GridLayoutRule's
	 * data. Ensures that, at the end of the method execution, all children of
	 * the Composite have a layout's data either null or of type GridData.
	 * 
	 */
	public static void platformSpecificRefresh(Object containerView,
			GridLayoutRule rule) {
		assert rule != null;
		if (!(containerView instanceof SWTContainerView))
			return;
		final Composite context = (Composite) ((SWTContainerView) containerView)
				.getSWTWidget();
		if (context == null || context.isDisposed())
			return;

		final Layout previousLayout = context.getLayout();
		context.setLayout(convertIntoSWTGridLayout(rule));

		// we check if we changed the type of the layout
		if (previousLayout == null && context.getLayout() == null)
			return;
		if (previousLayout != null
				&& context.getLayout() != null
				&& previousLayout.getClass().equals(
						context.getLayout().getClass()))
			return;

		for (Control child : context.getChildren())
			if (!(child.getLayoutData() instanceof GridData)) {
				Object data = child.getData(SWTWidgetView.WAZAABI_HOST_KEY);
				if (data instanceof AbstractComponentEditPart)
					child.setLayoutData(GridDataStyleRuleManager
							.getFirstGridData((AbstractComponentEditPart) data));
			}
	}
}
