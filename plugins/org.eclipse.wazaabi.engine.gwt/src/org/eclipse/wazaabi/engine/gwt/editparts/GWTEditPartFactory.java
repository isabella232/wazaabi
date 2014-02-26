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

package org.eclipse.wazaabi.engine.gwt.editparts;

import org.eclipse.wazaabi.engine.core.editparts.factories.EditPartFactory;
import org.eclipse.wazaabi.engine.core.gef.EditPart;


public class GWTEditPartFactory implements EditPartFactory {

    private static final String EDITPART_FACTORY_ID = "org.eclipse.wazaabi.engine.fx.editparts.GWTEditPartFactory";

    private EditPart getPartForElement(Object modelElement) {
        return null;
    }

    public Object createComponent(Object callingContext, Object model, Object creationHint) {
        // get EditPart for model element
        EditPart part = getPartForElement(model);
        if (part == null)
            return null;
        // store model element in EditPart
        part.setModel(model);
        return part;
    }

    public boolean isFactoryFor(Object callingContext, Object model, Object creationHint) {
        return false;
    }

    public String getFactoryID() {
        return EDITPART_FACTORY_ID;
    }
}