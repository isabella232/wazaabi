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

package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartFactory;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

public class TreePartFactory implements EditPartFactory {

	public EditPart createEditPart(EditPart context, Object model) {
		EditPart newEditPart = null;
		if (model instanceof EObject) {
			EClass eClass = ((EObject) model).eClass();
			if (CoreWidgetsPackage.Literals.CONTAINER == eClass)
				newEditPart = new ContainerTreeEditPart();
			else if (CoreWidgetsPackage.Literals.LABEL == eClass)
				newEditPart = new LabelTreeEditPart();
			else if (CoreWidgetsPackage.Literals.CONTAINER
					.isSuperTypeOf(eClass))
				newEditPart = new AbstractButtonTreeEditPart();
			else if (CoreWidgetsPackage.Literals.ABSTRACT_COMPONENT
					.isSuperTypeOf(eClass))
				newEditPart = new AbstractComponentTreeEditPart();
			else if (CoreStylesPackage.Literals.LAYOUT_RULE
					.isSuperTypeOf(eClass))
				newEditPart = new LayoutRuleTreeEditPart();
			else if (CoreStylesPackage.Literals.LAYOUT_DATA_RULE
					.isSuperTypeOf(eClass))
				newEditPart = new LayoutDataRuleTreeEditPart();
			else if (EDPHandlersPackage.Literals.BINDING == eClass)
				newEditPart = new BindingTreeEditPart();
			else if (EDPHandlersPackage.Literals.EVENT_HANDLER
					.isSuperTypeOf(eClass))
				newEditPart = new EventHandlerTreeEditPart();
			else if (EDPEventsPackage.Literals.PROPERTY_CHANGED_EVENT == eClass)
				newEditPart = new PathEventTreeEditPart();
			else if (EDPEventsPackage.Literals.CONTENT_CHANGED_EVENT == eClass)
				newEditPart = new PathEventTreeEditPart();
			else if (EDPEventsPackage.Literals.EVENT.isSuperTypeOf(eClass))
				newEditPart = new EventTreeEditPart();
			else if (EDPEventsPackage.Literals.EVENT.isSuperTypeOf(eClass))
				newEditPart = new EventTreeEditPart();

			else if (EDPHandlersPackage.Literals.STRING_PARAMETER == eClass)
				newEditPart = new StringParameterTreeEditPart();
		}
		if (newEditPart != null)
			newEditPart.setModel(model);
		return newEditPart;
	}

}
