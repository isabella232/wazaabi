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

package org.eclipse.wazaabi.engine.gwt.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.views.TextComponentView;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.Widget;


public class GWTTextComponentView extends GWTWidgetView implements TextComponentView {

    private class ModifyHandler implements ChangeHandler {
        public void onChange(ChangeEvent event) {
            ((TextComponent) getHost().getModel()).setText(((TextBox) event.getSource()).getText());
        }
    }

    private ModifyHandler modifyHandler = new ModifyHandler();


    public EClass getWidgetViewEClass() {
        return CoreWidgetsPackage.Literals.TEXT_COMPONENT;
    }

    protected Widget createGWTWidget(Panel parent, int index) {
        return createText(parent, index);
    }

    protected TextBox createText(Panel parent, int index) {
        TextBox t = new TextBox();
        t.addChangeHandler(modifyHandler);
        GWTLayoutUtil.addChild(t, parent, index);
        return t;
    }

    public void setText(String text) {
        ((TextBox) getGWTWidget()).setText(text == null ? "" : text); //$NON-NLS-1$
        revalidate();
    }

    public String getText() {
        return ((TextBox) getGWTWidget()).getText();
    }
}
