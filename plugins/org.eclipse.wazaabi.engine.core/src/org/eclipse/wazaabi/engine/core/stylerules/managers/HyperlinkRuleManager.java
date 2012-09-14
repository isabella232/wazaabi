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

package org.eclipse.wazaabi.engine.core.stylerules.managers;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart.StyleRuleManager;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.HyperlinkRule;

public class HyperlinkRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
//		boolean hasChanged = false;
		switch (notification.getFeatureID(HyperlinkRule.class)) {
		case CoreStylesPackage.HYPERLINK_RULE__PROPERTY_NAME:
			reCreateWidgetView();
			return;
		default:
			super.notifyChanged(notification);
		}
//		if (hasChanged
//				&& getHost().styleRuleUpdated(
//						(StyleRule) notification.getNotifier()))
//			reCreateWidgetView();
	}

}
