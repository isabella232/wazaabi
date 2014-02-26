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
import org.eclipse.wazaabi.engine.core.views.AbstractComponentView;
import org.eclipse.wazaabi.engine.core.views.ContainerView;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;


public class GWTContainerView extends GWTWidgetView implements ContainerView {

    public EClass getWidgetViewEClass() {
        return CoreWidgetsPackage.Literals.CONTAINER;
    }

    @Override
    protected Widget createGWTWidget(Panel parent, int index) {
        Panel panel = null;
        for (StyleRule sr : ((StyledElement) getHost().getModel()).getStyleRules()) {
            if (!"layout".equals(sr.getPropertyName()))
                continue;
//            if (sr instanceof HBoxRule) {
//                pane = new HBox(((HBoxRule) sr).getSpacing());
//            } else if (sr instanceof VBoxRule) {
//                pane = new VBox(((VBoxRule) sr).getSpacing());
//            } else if (sr instanceof BorderLayoutRule) {
//                // TODO
//            }
        }
        panel = new FlowPanel();
        if (panel != null)
            GWTLayoutUtil.addChild(panel, parent, index);
        return panel;
    }

    @Override
    public void add(WidgetView view, int index) {
        // first we create the widget
        super.add(view, index);
        // TODO ?
//        if (index != ((Pane) getContentPane()).getChildren() .length - 1)
//            if (view instanceof FXWidgetView)
//                reorderChild((FXWidgetView) view, index);
    }
    
    @Override
    protected boolean needReCreateWidgetView(StyleRule styleRule, Widget widget) {
//        if (styleRule instanceof HBoxRule && !(node instanceof HBox))
//            return true;
//        if (styleRule instanceof VBoxRule && !(node instanceof VBox))
//            return true;
//        if (styleRule instanceof BorderLayoutRule && !(node instanceof BorderPane))
//            return true;
        return false;
    }

    public void reorderChild(AbstractComponentView child, int index) {
    }

    public void refreshTabIndexes() {
    }
}