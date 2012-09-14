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
import org.eclipse.wazaabi.engine.core.views.RadioButtonView;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.RadioButton;

public class RadioButtonEditPart extends AbstractButtonEditPart {

	@Override
	public void notifyChanged(Notification notification) {
		if (getWidgetView() instanceof RadioButtonView) {
			switch (notification.getFeatureID(RadioButton.class)) {
			case CoreWidgetsPackage.RADIO_BUTTON__SELECTED:
				if (!CompareUtils.areEquals(
						((RadioButtonView) getWidgetView()).isSelected(),
						notification.getNewBooleanValue())) {
					((RadioButtonView) getWidgetView()).setSelected(notification
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
		((RadioButtonView) getWidgetView())
				.setSelected(((RadioButton) getModel()).isSelected());
		getWidgetView().fireWidgetViewRepainted();
	}

}
