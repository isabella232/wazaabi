/***********************************************************************************************************************
 * Copyright (c) 2008 Olivier Moises, 2014 Pavel Erofeev
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises - initial API and implementation
 *   Pavel Erofeev - rendering engine for JavaFX
***********************************************************************************************************************/

package org.eclipse.wazaabi.engine.fx.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.views.factories.WidgetViewFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;


public class FXWidgetViewFactory implements WidgetViewFactory {

    private static final String FACTORY_ID = FXWidgetViewFactory.class.getName();

    public Object createComponent(Object callingContext, Object model, Object creationHint) {
        if (model instanceof EditPart && ((EditPart) model).getModel() instanceof EObject) {
            EClass eClass = ((EObject) ((EditPart) model).getModel()).eClass();
//            if (eClass == CoreWidgetsPackage.Literals.LABEL)
//                return new FXLabelView();
            if (eClass == CoreWidgetsPackage.Literals.PUSH_BUTTON)
                return new FXPushButtonView();
//            if (eClass == CoreWidgetsPackage.Literals.TEXT_COMPONENT)
//                return new FXTextComponentView();
            if (eClass == CoreWidgetsPackage.Literals.CONTAINER)
                return new FXContainerView();
        }
        throw new RuntimeException();
    }

    public boolean isFactoryFor(Object callingContext, Object model, Object creationHint) {
        return model instanceof WidgetEditPart
                && ((WidgetEditPart) model).getModel() instanceof EObject
                && ((EObject) ((WidgetEditPart) model).getModel()).eClass()
                        .getEPackage() == CoreWidgetsPackage.eINSTANCE;
    }

    public String getFactoryID() {
        return FACTORY_ID;
    }
}
