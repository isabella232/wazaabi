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
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTControlView;

public class RowDataStyleRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(RowDataRule.class)) {
		case SWTStylesPackage.ROW_DATA_RULE__EXCLUDE:
			hasChanged = !CompareUtils.areEquals(
					notification.getOldBooleanValue(),
					notification.getNewBooleanValue());
			break;
		case SWTStylesPackage.ROW_DATA_RULE__HEIGHT:
		case SWTStylesPackage.ROW_DATA_RULE__WIDTH:
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

	/**
	 * Given a RowDataRule, creates a SWT RowData and sets its data using the
	 * RowDataRule.
	 * 
	 * @param rule
	 * @return Always a RowData
	 */
	private static RowData convertIntoSWTLayoutData(RowDataRule rule) {
		RowData rowData = new RowData();
		rowData.exclude = rule.isExclude();
		rowData.height = rule.getHeight();
		rowData.width = rule.getWidth();
		return rowData;
	}

	/**
	 * Synchronizes the given widgetView according with the given rule.If the
	 * container's layout is a RowLayout, the method sets the SWT Control's
	 * layout data with : the rule's data if the rule is not null, the first
	 * RowDataRule otherwise. If no RowDataRule is attached to the component,
	 * then the control's layout data is set to null.
	 * 
	 * @param widgetView
	 * @param rule
	 */
	public static void platformSpecificRefresh(Object widgetView,
			RowDataRule rule) {
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
		if (parent.getLayout() instanceof RowLayout)
			((Control) context.getSWTWidget())
					.setLayoutData(convertIntoSWTLayoutData(rule));
		else if (rule == null)
			((Control) context.getSWTWidget())
					.setLayoutData(getFirstRowData(context));
	}

	/**
	 * Returns a SWT RowData built by getting data from the first RowDataRule
	 * found in the list of style rules attached to this WidgetView's model.
	 * 
	 * @param widgetView
	 * @return
	 */
	protected static RowData getFirstRowData(SWTControlView widgetView) {
		return getFirstRowData((AbstractComponentEditPart) widgetView.getHost());
	}

	/**
	 * Returns a SWT RowData built by getting data from the first RowDataRule
	 * found in the list of style rules attached to this editPart's model.
	 * 
	 * @param editPart
	 *            A not null editPart
	 * @return
	 */
	static RowData getFirstRowData(AbstractComponentEditPart editPart) {
		assert editPart != null;
		for (StyleRule rule : ((StyledElement) editPart.getModel())
				.getStyleRules())
			if (rule instanceof RowDataRule)
				return convertIntoSWTLayoutData((RowDataRule) rule);
		return null;
	}

}
