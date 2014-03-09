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

import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.views.AbstractComponentView;
import org.eclipse.wazaabi.engine.core.views.ContainerView;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule;
import org.eclipse.wazaabi.mm.fx.styles.HBoxRule;
import org.eclipse.wazaabi.mm.fx.styles.VBoxRule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class FXContainerView extends FXWidgetView implements ContainerView {

    private static final Logger log = LoggerFactory.getLogger(FXContainerView.class);

    
    public EClass getWidgetViewEClass() {
        return CoreWidgetsPackage.Literals.CONTAINER;
    }

    @Override
    protected Node createFXNode(Pane parent, int index) {
        Pane pane = null;
        for (StyleRule sr : ((StyledElement) getHost().getModel()).getStyleRules()) {
            if (!"layout".equals(sr.getPropertyName()))
                continue;
            if (sr instanceof HBoxRule) {
                pane = new HBox(((HBoxRule) sr).getSpacing());
                break;
            } else if (sr instanceof VBoxRule) {
                pane = new VBox(((VBoxRule) sr).getSpacing());
                break;
            } else if (sr instanceof BorderLayoutRule) {
                // TODO
                break;
            } else if (sr instanceof BoxLayoutRule) {
                BoxLayoutRule blr = (BoxLayoutRule) sr;
                if (blr.getOrientation() == Orientation.HORIZONTAL)
                    pane = new HBox(blr.getSpacing());
                else
                    pane = new VBox(blr.getSpacing());
                pane.setPadding(new Insets(blr.getMargin()));
                break;
            }
        }
        if (pane != null)
            FXLayoutUtil.addChild(pane, parent, index);
        return pane;
    }

    @Override
    public void add(WidgetView view, int index) {
        // first we create the widget
        log.debug("adding widget {}, index {}", view, index);
        super.add(view, index);
        // TODO ?
//        if (index != ((Pane) getContentPane()).getChildren() .length - 1)
//            if (view instanceof FXWidgetView)
//                reorderChild((FXWidgetView) view, index);
    }
    
    @Override
    protected boolean needReCreateWidgetView(StyleRule styleRule, Node node) {
        if (styleRule instanceof HBoxRule && !(node instanceof HBox))
            return true;
        if (styleRule instanceof VBoxRule && !(node instanceof VBox))
            return true;
        if (styleRule instanceof BorderLayoutRule && !(node instanceof BorderPane))
            return true;
        if (styleRule instanceof BoxLayoutRule)
            return true;
        return false;
    }

    public void reorderChild(AbstractComponentView child, int index) {
// TODO ? 
//        if (!(((FXWidgetView) child).getSWTWidget() instanceof Control)
//                || ((FXWidgetView) child).getSWTWidget().isDisposed())
//            return;
//
//        // get the SWT Control child
//        final Node childControl = (Node) ((FXWidgetView) child).getFXNode();
//        // get the SWT Composite (this)
//        final Composite composite = (Composite) getContentPane();
//
//        if (childControl.getParent() != composite)
//            return;
//        int oldIndex = -1;
//        for (int i = 0; i < composite.getChildren().length; i++)
//            if (composite.getChildren()[i] == childControl) {
//                oldIndex = i;
//                break;
//            }
//        if (index == oldIndex)
//            return;
//
//        if (oldIndex < index)
//            childControl.moveBelow(composite.getChildren()[index]);
//        else
//            childControl.moveAbove(composite.getChildren()[index]);

    }

    public void refreshTabIndexes() {
    }
}
