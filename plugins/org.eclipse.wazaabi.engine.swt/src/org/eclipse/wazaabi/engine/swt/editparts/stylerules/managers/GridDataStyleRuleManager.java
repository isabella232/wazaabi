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
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.views.SWTControlView;

public class GridDataStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(GridDataRule.class)) {
		case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE:
		case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE:
		case SWTStylesPackage.GRID_DATA_RULE__EXCLUDE:
			hasChanged = !CompareUtils.areEquals(
					notification.getOldBooleanValue(),
					notification.getNewBooleanValue());
			break;
		case SWTStylesPackage.GRID_DATA_RULE__HEIGHT_HINT:
		case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_INDENT:
		case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_SPAN:
		case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_HEIGHT:
		case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_WIDTH:
		case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_INDENT:
		case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_SPAN:
		case SWTStylesPackage.GRID_DATA_RULE__WIDTH_HINT:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			break;
		case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT:
		case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_ALIGNEMENT:
			// TODO : check this, not sure about comparison of
			// GridDataAlignement
			hasChanged = notification.getOldValue() != notification
					.getNewValue();
			break;
		default:
			super.notifyChanged(notification);
		}
		if (hasChanged
				&& getHost().styleRuleUpdated(
						(StyleRule) notification.getNotifier()))
			reCreateWidgetView();
	}

	/**
	 * Given a GridDataRule, creates a SWT GridData and sets its data using the
	 * GridDataRule.
	 * 
	 * @param rule
	 * @return Always a GridData
	 */
	private static GridData convertIntoSWTGridData(GridDataRule rule) {
		GridData gridData = new GridData();
		if (rule != null) {
			gridData.exclude = rule.isExclude();
			gridData.grabExcessHorizontalSpace = rule
					.isGrabExcessHorizontalSpace();
			gridData.grabExcessVerticalSpace = rule.isGrabExcessVerticalSpace();
			gridData.heightHint = rule.getHeightHint();
			switch (rule.getHorizontalAlignement()) {
			case BEGINNING:
				gridData.horizontalAlignment = GridData.BEGINNING;
				break;
			case CENTER:
				gridData.horizontalAlignment = GridData.CENTER;
				break;
			case END:
				gridData.horizontalAlignment = GridData.END;
				break;
			case FILL:
				gridData.horizontalAlignment = GridData.FILL;
				break;
			}
			gridData.horizontalIndent = rule.getHorizontalIndent();
			gridData.horizontalSpan = rule.getHorizontalSpan();
			gridData.minimumHeight = rule.getMinimumHeight();
			gridData.minimumWidth = rule.getMinimumWidth();
			switch (rule.getVerticalAlignement()) {
			case BEGINNING:
				gridData.verticalAlignment = GridData.BEGINNING;
				break;
			case CENTER:
				gridData.verticalAlignment = GridData.CENTER;
				break;
			case END:
				gridData.verticalAlignment = GridData.END;
				break;
			case FILL:
				gridData.verticalAlignment = GridData.FILL;
				break;
			}

			gridData.verticalIndent = rule.getVerticalIndent();
			gridData.verticalSpan = rule.getVerticalSpan();
			gridData.widthHint = rule.getWidthHint();
		}
		return gridData;
	}

	/**
	 * Synchronizes the given widgetView according with the given rule.If the
	 * container's layout is a GridLayout, the method sets the SWT Control's
	 * layout data with : the rule's data if the rule is not null, the first
	 * GridDataRule otherwise. If no GridDataRule is attached to the component,
	 * then the control's layout data is set to null.
	 * 
	 * @param widgetView
	 * @param rule
	 */
	public static void platformSpecificRefresh(Object widgetView,
			GridDataRule rule) {
		if (!(widgetView instanceof SWTControlView))
			return;
		SWTControlView context = (SWTControlView) widgetView;
		if (!(context.getSWTWidget() instanceof Control)
				|| context.getSWTWidget().isDisposed())
			return;
		if (context.getParent() == null
				|| !(context.getParent().getSWTWidget() instanceof Composite))
			return;
		Composite parent = (Composite) context.getParent().getSWTWidget();
		if (parent.getLayout() instanceof GridLayout)
			((Control) context.getSWTWidget())
					.setLayoutData(convertIntoSWTGridData(rule));
		else if (rule == null)
			((Control) context.getSWTWidget())
					.setLayoutData(getFirstGridData(context));
	}

	/**
	 * Returns a SWT GridData built by getting data from the first GridDataRule
	 * found in the list of style rules attached to this WidgetView's model.
	 * 
	 * @param widgetView
	 * @return
	 */
	protected static GridData getFirstGridData(SWTControlView widgetView) {
		return getFirstGridData((AbstractComponentEditPart) widgetView
				.getHost());
	}

	/**
	 * Returns a SWT GridData built by getting data from the first GridDataRule
	 * found in the list of style rules attached to this editPart's model.
	 * 
	 * @param editPart
	 *            A not null editPart
	 * @return
	 */
	static GridData getFirstGridData(AbstractComponentEditPart editPart) {
		assert editPart != null;
		for (StyleRule rule : ((StyledElement) editPart.getModel())
				.getStyleRules())
			if (rule instanceof GridDataRule)
				return convertIntoSWTGridData((GridDataRule) rule);
		return null;
	}

}
