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
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.StackPane;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.core.views.AbstractComponentView;
import org.eclipse.wazaabi.engine.core.views.ContainerView;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.Position;
import org.eclipse.wazaabi.mm.core.styles.BlankRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;


public class FXContainerView extends FXWidgetView implements ContainerView {

    public EClass getWidgetViewEClass() {
        return CoreWidgetsPackage.Literals.CONTAINER;
        //return SWTDescriptorsPackage.Literals.COMPOSITE;
    }

    @Override
    protected Node createFXNode(Node parent, int swtStyle, int index) {
        assert parent instanceof StackPane;

        GridPane grid = new GridPane();
        grid.setAlignment(Pos.CENTER);
        grid.setHgap(10);
        grid.setVgap(10);
        grid.setPadding(new Insets(25, 25, 25, 25));
        ((StackPane) parent).getChildren().add(grid);

        return grid;
    }

    private LayoutRule currentLayoutRule = null;

    protected void setLayout(LayoutRule rule) {
        if (!(rule instanceof BlankRule))
            currentLayoutRule = rule;
        else
            currentLayoutRule = (LayoutRule) ((StyledElement) getHost()
                    .getModel()).getFirstStyleRule(
                    ContainerEditPart.LAYOUT_PROPERTY_NAME,
                    CoreStylesPackage.Literals.LAYOUT_RULE);

        if (currentLayoutRule != null)
            platformSpecificRefreshStyleRule(this, currentLayoutRule);
        else
            // TODO ? ((Pane) getContentPane()).;
            ;//((Composite) getContentPane()).setLayout(null);
        revalidate();
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
