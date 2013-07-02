/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.forms.stylerules.managers;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.core.stylerules.managers.FontRuleManager;
import org.eclipse.wazaabi.engine.swt.forms.editparts.LabelEditPart;
import org.eclipse.wazaabi.engine.swt.forms.views.SWTLabelView;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.FontRule;

public class XMLFontRuleManager extends FontRuleManager {

	@Override
	public void notifyChanged(Notification notification) {
		assert getHost() != null;
		// we focus only on SWTLabelView
		if (!(getHost().getWidgetView() instanceof SWTLabelView)) {
			super.notifyChanged(notification);
			return;
		}
		SWTLabelView labelView = (SWTLabelView) getHost().getWidgetView();
		// if the label is not a FormText, we delegate to ancestor
		if (!(labelView.getSWTWidget() instanceof FormText)) {
			super.notifyChanged(notification);
			return;
		}

		if (notification.getEventType() != Notification.SET)
			return;
		if (notification.getFeatureID(FontRule.class) == CoreStylesPackage.FONT_RULE__PROPERTY_NAME) {
			// first we test if we switch from or to control's font
			if (AbstractComponentEditPart.FONT_PROPERTY_NAME
					.equals(notification.getOldStringValue())
					|| AbstractComponentEditPart.FONT_PROPERTY_NAME
							.equals(notification.getNewStringValue()))
				reCreateWidgetView();
			// we get the key values (old and new ones)
			String oldKey = null;
			String newKey = null;
			if (notification.getOldStringValue() != null
					&& notification.getOldStringValue().length() > LabelEditPart._KEY_PREFIX_LENGHT
					&& notification.getOldStringValue().startsWith(
							LabelEditPart._KEY_PREFIX))
				oldKey = notification.getOldStringValue().substring(
						LabelEditPart._KEY_PREFIX_LENGHT);
			if (notification.getNewStringValue() != null
					&& notification.getNewStringValue().length() > LabelEditPart._KEY_PREFIX_LENGHT
					&& notification.getNewStringValue().startsWith(
							LabelEditPart._KEY_PREFIX))
				newKey = notification.getNewStringValue().substring(
						LabelEditPart._KEY_PREFIX_LENGHT);
			if (oldKey == null) {
				if (newKey == null)
					return;
			} else if (oldKey.equals(newKey))
				return;
			if (oldKey != null)
				labelView.removeXMLFont(oldKey);
			if (newKey != null)
				labelView.setXMLFont(newKey,
						(FontRule) notification.getNotifier());
		} else
			super.notifyChanged(notification);
	}
}
