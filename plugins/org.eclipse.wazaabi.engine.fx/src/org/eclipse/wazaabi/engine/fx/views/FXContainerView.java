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
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Widget;
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
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;


public class FXContainerView extends FXWidgetView implements ContainerView {

    public EClass getWidgetViewEClass() {
        return SWTDescriptorsPackage.Literals.COMPOSITE;
    }

    protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
        return new Composite((Composite) parent, computeSWTCreationStyle(getHost()));
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
            ((Composite) getContentPane()).setLayout(null);
        revalidate();
    }

    @Override
    protected int computeSWTCreationStyle(StyleRule rule) {
        if (ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
                .getPropertyName()) && rule instanceof SashFormLayoutRule) {
            if (((SashFormLayoutRule) rule).getOrientation() == Orientation.VERTICAL)
                return SWT.VERTICAL;
            return SWT.HORIZONTAL;
        } else if (ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
                .getPropertyName()) && rule instanceof TabbedLayoutRule)
            return ((TabbedLayoutRule) rule).getPosition() == Position.TOP ? SWT.TOP
                    : SWT.BOTTOM;
        return super.computeSWTCreationStyle(rule);
    }

    @Override
    public void add(WidgetView view, int index) {
        // first we create the widget
        super.add(view, index);
        if (index != ((Composite) getContentPane()).getChildren().length - 1)
            if (view instanceof FXWidgetView)
                reorderChild((FXWidgetView) view, index);
    }

    public void reorderChild(AbstractComponentView child, int index) {

        if (!(((FXWidgetView) child).getSWTWidget() instanceof Control)
                || ((FXWidgetView) child).getSWTWidget().isDisposed())
            return;

        // get the SWT Control child
        final Control childControl = (Control) ((FXWidgetView) child)
                .getSWTWidget();
        // get the SWT Composite (this)
        final Composite composite = (Composite) getContentPane();

        if (childControl.getParent() != composite)
            return;
        int oldIndex = -1;
        for (int i = 0; i < composite.getChildren().length; i++)
            if (composite.getChildren()[i] == childControl) {
                oldIndex = i;
                break;
            }
        if (index == oldIndex)
            return;

        if (oldIndex < index)
            childControl.moveBelow(composite.getChildren()[index]);
        else
            childControl.moveAbove(composite.getChildren()[index]);

    }

    public void refreshTabIndexes() {
    }
}
