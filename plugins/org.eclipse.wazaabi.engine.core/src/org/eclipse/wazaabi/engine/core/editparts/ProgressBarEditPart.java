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
import org.eclipse.wazaabi.engine.core.views.ProgressBarView;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.ProgressBar;

public class ProgressBarEditPart extends AbstractComponentEditPart {

	public static final String MAXIMUM_PROPERTY_NAME = "maximum"; //$NON-NLS-1$
	public static final String MINIMUM_PROPERTY_NAME = "minimum"; //$NON-NLS-1$
	public static final String ORIENTATION_PROPERTY_NAME = "orientation"; //$NON-NLS-1$

	public EClass getModelEClass() {
		return CoreWidgetsPackage.Literals.PROGRESS_BAR;
	}

	@Override
	public void notifyChanged(Notification notification) {
		if (getWidgetView() instanceof ProgressBarView) {
			switch (notification.getFeatureID(ProgressBar.class)) {
			case CoreWidgetsPackage.PROGRESS_BAR__VALUE:
				if (!CompareUtils.areEquals(
						((ProgressBarView) getWidgetView()).getValue(),
						notification.getNewIntValue())) {
						((ProgressBarView) getWidgetView())
								.setValue(notification.getNewIntValue());
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
		ProgressBarView view = ((ProgressBarView) getWidgetView());

		refreshUniqueStyleRule(MAXIMUM_PROPERTY_NAME);
		refreshUniqueStyleRule(MINIMUM_PROPERTY_NAME);
		refreshUniqueStyleRule(ORIENTATION_PROPERTY_NAME);

		view.setValue(((ProgressBar) getModel()).getValue());

		// view.updatePlatformSpecificStyleProperties(((StyledElement)
		// getModel())
		// .getStyleRules());
	}
}
