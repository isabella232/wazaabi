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

package org.eclipse.wazaabi.engine.core.editparts;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.wazaabi.engine.core.views.CheckBoxView;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.mm.core.widgets.CheckBox;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class CheckBoxEditPart extends AbstractButtonEditPart {
	@Override
	public void notifyChanged(Notification notification) {
		if (getWidgetView() instanceof CheckBoxView) {
			switch (notification.getFeatureID(CheckBox.class)) {
			case CoreWidgetsPackage.RADIO_BUTTON__SELECTED:
				if (!CompareUtils.areEquals(
						((CheckBoxView) getWidgetView()).isSelected(),
						notification.getNewBooleanValue())) {
					((CheckBoxView) getWidgetView()).setSelected(notification
							.getNewBooleanValue());
					getWidgetView().fireWidgetViewRepainted();
				}
				break;
			default:
				super.notifyChanged(notification);
			}
		}
	}

	public void refreshFeaturesAndStyles() {
		super.refreshFeaturesAndStyles();
		((CheckBoxView) getWidgetView()).setSelected(((CheckBox) getModel())
				.isSelected());
		getWidgetView().fireWidgetViewRepainted();
	}

}
