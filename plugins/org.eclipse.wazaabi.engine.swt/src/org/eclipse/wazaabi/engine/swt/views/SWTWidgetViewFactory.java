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

package org.eclipse.wazaabi.engine.swt.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.engine.swt.viewers.AbstractSWTViewer;
import org.eclipse.wazaabi.engine.swt.views.collections.SWTCollectionView;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class SWTWidgetViewFactory implements WidgetViewFactory {

	public WidgetView createWidgetView(WidgetEditPart editPart,
			Object creationHint) {
		if (editPart != null && editPart.getModel() instanceof EObject) {
			EClass eClass = ((EObject) editPart.getModel()).eClass();
			if (eClass == CoreWidgetsPackage.Literals.PROGRESS_BAR)
				return new SWTProgressBarView();
			if (eClass == CoreWidgetsPackage.Literals.LABEL)
				return new SWTLabelView();
			if (eClass == CoreWidgetsPackage.Literals.SEPARATOR)
				return new SWTSeparatorView();
			if (eClass == CoreWidgetsPackage.Literals.PUSH_BUTTON)
				return new SWTPushButtonView();
			if (eClass == CoreWidgetsPackage.Literals.RADIO_BUTTON)
				return new SWTRadioButtonView();
			if (eClass == CoreWidgetsPackage.Literals.CHECK_BOX)
				return new SWTCheckBoxView();
			if (eClass == CoreWidgetsPackage.Literals.TEXT_COMPONENT)
				return new SWTTextComponentView();
			if (eClass == CoreWidgetsPackage.Literals.SLIDER)
				return new SWTSliderView();
			if (eClass == CoreWidgetsPackage.Literals.SCALE)
				return new SWTScaleView();
			if (eClass == CoreWidgetsPackage.Literals.SPINNER)
				return new SWTSpinnerView();
			if (eClass == CoreWidgetsPackage.Literals.COLLECTION)
				return new SWTCollectionView();
			if (eClass == CoreWidgetsPackage.Literals.CONTAINER)
				return new SWTContainerView();
			if (eClass == CoreWidgetsPackage.Literals.MENU_COMPONENT)
				return new SWTMenuComponentView();
		}
		return null;
	}

	public boolean isFactoryFor(Object type) {
		if (type instanceof AbstractSWTViewer)
			return true;
		return false;
	}

}
