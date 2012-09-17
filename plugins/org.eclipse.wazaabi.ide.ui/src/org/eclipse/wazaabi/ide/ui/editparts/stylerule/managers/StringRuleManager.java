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

package org.eclipse.wazaabi.ide.ui.editparts.stylerule.managers;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.ide.ui.editparts.AbstractComponentTreeEditPart.StyleRuleManager;

public class StringRuleManager extends StyleRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		if (notification.getEventType() != Notification.SET)
			return;
		switch (notification.getFeatureID(StringRule.class)) {
		case CoreStylesPackage.STRING_RULE__VALUE:
			if (!CompareUtils.areEquals(notification.getOldStringValue(),
					notification.getNewStringValue()))
				getHost().styleRuleUpdated(
						(StyleRule) notification.getNotifier());
			break;
		default:
			super.notifyChanged(notification);
		}
	}
}
