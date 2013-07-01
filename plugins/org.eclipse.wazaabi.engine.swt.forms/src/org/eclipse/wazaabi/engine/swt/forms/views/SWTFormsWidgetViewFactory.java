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

package org.eclipse.wazaabi.engine.swt.forms.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.swt.forms.views.collections.SWTCollectionView;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class SWTFormsWidgetViewFactory extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetViewFactory {

	// private final Logger logger = LoggerFactory
	// .getLogger(SWTWidgetViewFactory.class);

	public static final String FACTORY_ID = SWTFormsWidgetViewFactory.class
			.getName();

	@Override
	public Object createComponent(Object callingContext, Object model,
			Object creationHint) {

		if (model instanceof WidgetEditPart
				&& ((EditPart) model).getModel() instanceof EObject) {
			EClass eClass = ((EObject) ((EditPart) model).getModel()).eClass();
			SWTContainerView containingForm = SWTFormsUtils
					.getSWTContainer((WidgetEditPart) model);
			if (containingForm != null) {
				if (eClass == CoreWidgetsPackage.Literals.LABEL)
					return new SWTLabelView(containingForm);
				if (eClass == CoreWidgetsPackage.Literals.SEPARATOR)
					return new SWTSeparatorView(containingForm);
				if (eClass == CoreWidgetsPackage.Literals.PUSH_BUTTON)
					return new SWTPushButtonView(containingForm);
				if (eClass == CoreWidgetsPackage.Literals.RADIO_BUTTON)
					return new SWTRadioButtonView(containingForm);
				if (eClass == CoreWidgetsPackage.Literals.CHECK_BOX)
					return new SWTCheckBoxView(containingForm);
				if (eClass == CoreWidgetsPackage.Literals.TEXT_COMPONENT)
					return new SWTTextComponentView(containingForm);

				// when 'simple' container is created inside a Form
				if (eClass == CoreWidgetsPackage.Literals.CONTAINER)
					return new SWTContainerView(containingForm);
				if (eClass == CoreWidgetsPackage.Literals.COLLECTION)
					return new SWTCollectionView(containingForm);
			} else if (eClass == CoreWidgetsPackage.Literals.CONTAINER)
				return new SWTContainerView(null);

			return super.createComponent(callingContext, model, creationHint);

		}
		return null;
	}

	@Override
	public String getFactoryID() {
		return FACTORY_ID;
	}

}
