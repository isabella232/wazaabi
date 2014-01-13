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

import javafx.scene.Node;
import javafx.scene.Scene;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.wazaabi.engine.core.editparts.AbstractWidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.gef.RootEditPart;
import org.eclipse.wazaabi.engine.core.impl.CoreRegistryImpl;
import org.eclipse.wazaabi.engine.core.viewers.AbstractEditPartViewer;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.fx.editparts.FXRootEditPart;
import org.eclipse.wazaabi.engine.fx.views.FXWidgetView;
import org.eclipse.wazaabi.engine.fx.views.updman.DeferredUpdateManager;
import org.eclipse.wazaabi.engine.fx.views.updman.UpdateManager;


public class FXViewer extends AbstractEditPartViewer {

    private final UpdateManager manager = new DeferredUpdateManager();
    private final Scene scene;

    public FXViewer(Scene scene) {
        this(scene, new FXRootEditPart());
    }

    public FXViewer(Scene scene, FXRootEditPart rootEditPart) {
        this.scene = scene;
        setRootEditPart(rootEditPart);
    }

    public UpdateManager getUpdateManager() {
        return manager;
    }

    public Node getParent() {
        return scene.getRoot();
    }

    protected Node getWidget() {
        if (!(getContents() instanceof AbstractWidgetEditPart))
            return null;
        if (((AbstractWidgetEditPart) getContents()).getWidgetView() instanceof FXWidgetView)
            return ((FXWidgetView) ((AbstractWidgetEditPart) getContents()).getWidgetView()).getFXNode();
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
    public Node getControl() {
        return (Node) getWidget();
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
