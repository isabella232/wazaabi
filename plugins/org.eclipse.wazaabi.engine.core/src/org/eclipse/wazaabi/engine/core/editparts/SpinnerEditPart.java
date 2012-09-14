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
import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.views.SpinnerView;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Spinner;

public class SpinnerEditPart extends AbstractComponentEditPart {
	public static final String MAXIMUM_PROPERTY_NAME = "maximum"; //$NON-NLS-1$
	public static final String MINIMUM_PROPERTY_NAME = "minimum"; //$NON-NLS-1$
	public static final String DIGITS_PROPERTY_NAME = "digits"; //$NON-NLS-1$
	public static final String INCREMENT_PROPERTY_NAME = "increment"; //$NON-NLS-1$
	public static final String TEXTLIMIT_PROPERTY_NAME = "textlimit"; //$NON-NLS-1$

	public EClass getModelEClass() {
		return CoreWidgetsPackage.Literals.SPINNER;
	}

	@Override
	public void notifyChanged(Notification notification) {
		if (getWidgetView() instanceof SpinnerView) {
			switch (notification.getFeatureID(Spinner.class)) {
			case CoreWidgetsPackage.SPINNER__VALUE:
				if (!CompareUtils.areEquals(
						((SpinnerView) getWidgetView()).getValue(),
						notification.getNewIntValue())) {
					((SpinnerView) getWidgetView()).setValue(notification
							.getNewIntValue());
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
		SpinnerView view = ((SpinnerView) getWidgetView());

		refreshUniqueStyleRule(MAXIMUM_PROPERTY_NAME);
		refreshUniqueStyleRule(MINIMUM_PROPERTY_NAME);
		refreshUniqueStyleRule(DIGITS_PROPERTY_NAME);
		refreshUniqueStyleRule(INCREMENT_PROPERTY_NAME);
		refreshUniqueStyleRule(TEXTLIMIT_PROPERTY_NAME);

		view.setValue(((Spinner) getModel()).getValue());

	}
}
