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


import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.viewers.AbstractWidgetRootEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.gwt.viewers.GWTViewer;
import org.eclipse.wazaabi.engine.gwt.views.GWTWidgetView;

import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;


public class GWTRootEditPart extends AbstractWidgetRootEditPart {

    private GWTWidgetView view = new GWTWidgetView() {
        protected Widget createGWTWidget(Panel parent, int index) {
            throw new UnsupportedOperationException();
        }

        @Override
        public Widget getGWTWidget() {
            if (getViewer() != null)
                return ((GWTViewer) getViewer()).getParent();
            return null;
        }

        public EClass getWidgetViewEClass() {
            return null;
        }

        @Override
        public WidgetEditPart getHost() {
            return GWTRootEditPart.this;
        }

        @Override
        protected boolean isValidationRoot() {
            return true;
        }
    };

    public WidgetView getWidgetView() {
        return view;
    }
}
