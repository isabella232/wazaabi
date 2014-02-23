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

package org.eclipse.wazaabi.engine.fx.editparts;

import javafx.scene.Node;
import javafx.scene.layout.Pane;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.viewers.AbstractWidgetRootEditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.fx.viewers.FXViewer;
import org.eclipse.wazaabi.engine.fx.views.FXWidgetView;


public class FXRootEditPart extends AbstractWidgetRootEditPart {

    private FXWidgetView view = new FXWidgetView() {
        protected Node createFXNode(Pane parent, int index) {
            throw new UnsupportedOperationException();
        }

        @Override
        public Node getFXNode() {
            if (getViewer() != null)
                return ((FXViewer) getViewer()).getParent();
            return null;
        }

        public EClass getWidgetViewEClass() {
            return null;
        }

        @Override
        public WidgetEditPart getHost() {
            return FXRootEditPart.this;
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
