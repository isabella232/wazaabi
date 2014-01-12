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

package org.eclipse.wazaabi.engine.fx.viewers;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.gef.RootEditPart;
import org.eclipse.wazaabi.engine.core.impl.CoreRegistryImpl;
import org.eclipse.wazaabi.engine.core.viewers.AbstractEditPartViewer;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.fx.editparts.FXRootEditPart;
import org.eclipse.wazaabi.engine.fx.views.FXWidgetView;
import org.eclipse.wazaabi.engine.swt.commons.views.DeferredUpdateManager;
import org.eclipse.wazaabi.engine.swt.commons.views.UpdateManager;


public class FXViewer extends AbstractEditPartViewer {

    private UpdateManager manager = new DeferredUpdateManager();

    private final org.eclipse.swt.widgets.Composite parent;

    public FXViewer(org.eclipse.swt.widgets.Composite parent) {
        this(parent, new FXRootEditPart());
    }

    public FXViewer(org.eclipse.swt.widgets.Composite parent, FXRootEditPart rootEditPart) {
        this.parent = parent;
        setRootEditPart(rootEditPart);
    }

    public UpdateManager getUpdateManager() {
        return manager;
    }

    public org.eclipse.swt.widgets.Composite getParent() {
        return parent;
    }

    protected Widget getWidget() {
        if (!(getContents() instanceof AbstractWidgetEditPart))
            return null;
        if (((AbstractWidgetEditPart) getContents()).getWidgetView() instanceof FXWidgetView) {
            return ((FXWidgetView) ((AbstractWidgetEditPart) getContents())
                    .getWidgetView()).getSWTWidget();
        }
        return null;
    }

    protected Registry createRegistry() {
        return new CoreRegistryImpl();
    }

    // PROPOSAL : not sure to keep it
    protected static org.eclipse.wazaabi.mm.core.widgets.AbstractComponent getModelComponent(String uri) {
        Resource resource = new ResourceSetImpl().getResource( URI.createURI(uri), true);
        if (resource.getEObject("/") instanceof org.eclipse.wazaabi.mm.core.widgets.AbstractComponent)
            return (org.eclipse.wazaabi.mm.core.widgets.AbstractComponent) resource .getEObject("/");
        return null;
    }

    @Override
    public Control getControl() {
        return (Control) getWidget();
    }

    @Override
    public EditPart getContents() {
        if (getRootEditPart() == null)
            return null;
        return getRootEditPart().getContents();
    }

    @Override
    public void setRootEditPart(RootEditPart editpart) {
        assert editpart == null || editpart instanceof FXRootEditPart;
        super.setRootEditPart(editpart);
        if (!getRootEditPart().isActive())
            getRootEditPart().activate();
    }
}
