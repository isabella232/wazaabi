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
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.wazaabi.engine.core.CompareUtils;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;

public class TabbedLayoutRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		boolean hasChanged = false;
		switch (notification.getFeatureID(TabbedLayoutRule.class)) {
		case CoreStylesPackage.TABBED_LAYOUT_RULE__TOP:
			if (getHost().getWidgetView() instanceof SWTContainerView
					&& ((SWTContainerView) getHost().getWidgetView())
							.getSWTWidget() instanceof CTabFolder) {
				CTabFolder tabFolder = (CTabFolder) ((SWTContainerView) (getHost()
						.getWidgetView())).getSWTWidget();
				if (tabFolder.getSelectionIndex() != notification
						.getNewIntValue())
					tabFolder.setSelection(notification.getNewIntValue());
			}
			break;

		case CoreStylesPackage.TABBED_LAYOUT_RULE__POSITION:
			hasChanged = !CompareUtils.areEquals(notification.getOldIntValue(),
					notification.getNewIntValue());
			// the topComponent could not have changed while the topControl has
			// changed
			if (!hasChanged && getHost() instanceof ContainerEditPart) {
				hasChanged = true;
			}
			break;
		case CoreStylesPackage.TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE:
		case CoreStylesPackage.TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE:
		case CoreStylesPackage.TABBED_LAYOUT_RULE__MARGIN_HEIGHT:
		case CoreStylesPackage.TABBED_LAYOUT_RULE__MARGIN_WIDTH:
			hasChanged = !CompareUtils.areEquals(
					(Boolean) notification.getOldValue(),
					(Boolean) notification.getNewValue());
			break;
		case CoreStylesPackage.TABBED_LAYOUT_RULE__PROPERTY_NAME:
			reCreateWidgetView();
			return;
		default:
			super.notifyChanged(notification);
		}
		if (hasChanged
				&& getHost().styleRuleUpdated(
						(StyleRule) notification.getNotifier()))
			reCreateWidgetView();
	}

}
