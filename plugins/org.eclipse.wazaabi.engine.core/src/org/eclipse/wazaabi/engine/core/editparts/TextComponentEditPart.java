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
import org.eclipse.wazaabi.engine.core.views.TextComponentView;
import org.eclipse.wazaabi.engine.edp.CompareUtils;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;

public class TextComponentEditPart extends AbstractComponentEditPart {

	public static final String ORIENTATION_PROPERTY_NAME = "orientation"; //$NON-NLS-1$
	public static final String MULTI_LINE_PROPERTY_NAME = "multi-line"; //$NON-NLS-1$

	public EClass getModelEClass() {
		return CoreWidgetsPackage.Literals.TEXT_COMPONENT;
	}

	@Override
	public void notifyChanged(Notification notification) {
		if (getWidgetView() instanceof TextComponentView) {
			switch (notification.getFeatureID(TextComponent.class)) {
			case CoreWidgetsPackage.TEXT_COMPONENT__TEXT:
				if (!CompareUtils.areEquals(
						((TextComponentView) getWidgetView()).getText(),
						notification.getNewStringValue())) {
					((TextComponentView) getWidgetView()).setText(notification
							.getNewStringValue());
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
		((TextComponentView) getWidgetView())
				.setText(((TextComponent) getModel()).getText());
		getWidgetView().fireWidgetViewRepainted();
	}
}
