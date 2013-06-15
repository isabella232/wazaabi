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

package org.eclipse.wazaabi.ui.runtime.parts.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView;
import org.eclipse.wazaabi.ui.model.parts.PartsPackage;

public class PartsWidgetViewFactory implements WidgetViewFactory {

	public static final String FACTORY_ID = PartsWidgetViewFactory.class
			.getName();

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {
		if (model instanceof WidgetEditPart
				&& ((WidgetEditPart) model).getModel() instanceof EObject) {
			EClass eClass = ((EObject) ((WidgetEditPart) model).getModel())
					.eClass();
			if (eClass == PartsPackage.Literals.PAGE)
				return new SWTContainerView();
		}
		return null;
	}

	@Override
	public boolean isFactoryFor(Object callingContext, Object model, Object creationHint) {
		return model instanceof WidgetEditPart
				&& ((WidgetEditPart) model).getModel() instanceof EObject
				&& ((EObject) ((WidgetEditPart) model).getModel()).eClass()
						.getEPackage() == PartsPackage.eINSTANCE;
	}

	@Override
	public String getFactoryID() {
		return FACTORY_ID;
	}

}
