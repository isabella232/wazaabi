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

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.Node;
import javafx.scene.control.TextField;
import javafx.scene.layout.Pane;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.views.TextComponentView;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;


public class FXTextComponentView extends FXWidgetView implements TextComponentView {

    private class ModifyListener implements ChangeListener<String> {
        public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
            if (!newValue.equals(((TextComponent) getHost().getModel()).getText()))
                ((TextComponent) getHost().getModel()).setText(newValue);
        }
    }

    private ModifyListener modifyListener = new ModifyListener();


    public EClass getWidgetViewEClass() {
        return CoreWidgetsPackage.Literals.TEXT_COMPONENT;
    }

    protected Node createFXNode(Pane parent, int index) {
        return createText(parent, index);
    }

    protected TextField createText(Pane parent, int index) {
        TextField t = new TextField("Some initial text");
        t.textProperty().addListener(modifyListener);
        FXLayoutUtil.addChild(t, parent, index);
        return t;
    }

    public void setText(String text) {
        ((TextField) getFXNode()).setText(text == null ? "" : text); //$NON-NLS-1$
        revalidate();
    }

    public String getText() {
        return ((TextField) getFXNode()).getText();
    }
}
