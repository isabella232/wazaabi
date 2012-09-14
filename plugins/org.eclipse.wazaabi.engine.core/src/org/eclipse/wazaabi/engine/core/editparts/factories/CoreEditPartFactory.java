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

package org.eclipse.wazaabi.engine.core.editparts.factories;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.CheckBoxEditPart;
import org.eclipse.wazaabi.engine.core.editparts.CollectionEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.editparts.LabelEditPart;
import org.eclipse.wazaabi.engine.core.editparts.MenuComponentEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ProgressBarEditPart;
import org.eclipse.wazaabi.engine.core.editparts.PushButtonEditPart;
import org.eclipse.wazaabi.engine.core.editparts.RadioButtonEditPart;
import org.eclipse.wazaabi.engine.core.editparts.ScaleEditPart;
import org.eclipse.wazaabi.engine.core.editparts.SliderEditPart;
import org.eclipse.wazaabi.engine.core.editparts.SpinnerEditPart;
import org.eclipse.wazaabi.engine.core.editparts.TextComponentEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class CoreEditPartFactory implements EditPartFactory {

	// SWTWidgetViewFactory widgetViewFactory = new SWTWidgetViewFactory();

	private static final String EDITPART_FACTORY_ID = "org.eclipse.wazaabi.engine.core.editparts.factories.CoreEditPartFactory"; // $NON-NLs-1$

	public EditPart createEditPart(EditPart context, Object modelElement) {
		// get EditPart for model element
		EditPart part = getPartForElement(modelElement);
		if (part == null)
			return null;
		// store model element in EditPart
		part.setModel(modelElement);
		return part;
	}

	/**
	 * Maps an object to an EditPart.
	 *
	 * @throws RuntimeException
	 *             if no match was found (programming error)
	 */
	private EditPart getPartForElement(Object modelElement) {

		if (modelElement instanceof EObject) {
			EClass eClass = ((EObject) modelElement).eClass();
			if (eClass == CoreWidgetsPackage.Literals.PROGRESS_BAR)
				return new ProgressBarEditPart();
			if (eClass == CoreWidgetsPackage.Literals.LABEL)
				return new LabelEditPart();
			if (eClass == CoreWidgetsPackage.Literals.PUSH_BUTTON)
				return new PushButtonEditPart();
			if (eClass == CoreWidgetsPackage.Literals.RADIO_BUTTON)
				return new RadioButtonEditPart();
			if (eClass == CoreWidgetsPackage.Literals.CHECK_BOX)
				return new CheckBoxEditPart();
			if (eClass == CoreWidgetsPackage.Literals.TEXT_COMPONENT)
				return new TextComponentEditPart();
			if (eClass == CoreWidgetsPackage.Literals.SLIDER)
				return new SliderEditPart();
			if (eClass == CoreWidgetsPackage.Literals.SCALE)
				return new ScaleEditPart();
			if (eClass == CoreWidgetsPackage.Literals.SPINNER)
				return new SpinnerEditPart();
			if (eClass == CoreWidgetsPackage.Literals.COLLECTION)
				return new CollectionEditPart();
			if (eClass == CoreWidgetsPackage.Literals.CONTAINER)
				return new ContainerEditPart();
			if (eClass == CoreWidgetsPackage.Literals.MENU_COMPONENT)
				return new MenuComponentEditPart();
		}
		return null;
	}

	public String getId() {
		return EDITPART_FACTORY_ID;
	}

}
