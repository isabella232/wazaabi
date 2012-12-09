/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.dnd.AbstractTransferDragSourceListener;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.ui.part.PluginTransfer;
import org.eclipse.ui.part.PluginTransferData;

public class TreeViewerTransferDragListener extends AbstractTransferDragSourceListener {

    private List<EObject> modelSelection;

    public TreeViewerTransferDragListener(EditPartViewer viewer) {
        super(viewer, PluginTransfer.getInstance());
    }

    @SuppressWarnings("unchecked")
    public void dragSetData(DragSourceEvent event) {

        List<ModelDescriptor> modelDescriptors = new ArrayList<ModelDescriptor>();
        for (EditPart ep : (List<EditPart>) getViewer().getSelectedEditParts())
            if (ep.getModel() instanceof EObject) {
                ModelDescriptor modelDescriptor = ModelDescriptor
                        .createModelDescriptor((EObject) ep.getModel());
                if (modelDescriptor != null)
                    modelDescriptors.add(modelDescriptor);
            }
        if (ModelDescriptorTransfert.getInstance().isSupportedType(
                event.dataType))
            event.data = modelDescriptors.toArray(new ModelDescriptor[] {});
        else if (PluginTransfer.getInstance().isSupportedType(event.dataType)) {
            byte[] data = ModelDescriptorTransfert.getInstance().toByteArray(
                    modelDescriptors.toArray(new ModelDescriptor[] {}));
            event.data = new PluginTransferData(
                    "org.eclipse.wazaabi.ide.ui.editors.actions.DropActionDelegate", // TODO
                                                                                        // :
                                                                                        // put
                                                                                        // a
                                                                                        // constant
                    data);
        }
    }

    @SuppressWarnings("unchecked")
    public void dragStart(DragSourceEvent event) {
        // TreeViewerTransfer.getInstance().setViewer(getViewer());
        List<?> selection = getViewer().getSelectedEditParts();
        // TreeViewerTransfer.getInstance().setObject(selection);
        saveModelSelection((List<EditPart>) selection);
    }

    public void dragFinished(DragSourceEvent event) {
        // TreeViewerTransfer.getInstance().setObject(null);
        // TreeViewerTransfer.getInstance().setViewer(null);
        if (event.doit)
            revertModelSelection();
        else
            modelSelection = null;
    }

    protected void revertModelSelection() {
        List<EditPart> list = new ArrayList<EditPart>();
        Object editpart;
        for (int i = 0; i < modelSelection.size(); i++) {
            editpart = getViewer().getEditPartRegistry().get(
                    modelSelection.get(i));
            if (editpart != null)
                list.add((EditPart) editpart);
        }
        getViewer().setSelection(new StructuredSelection(list));
        modelSelection = null;
    }

    protected void saveModelSelection(List<EditPart> editPartSelection) {
        modelSelection = new ArrayList<EObject>();
        for (int i = 0; i < editPartSelection.size(); i++) {
            EditPart editpart = (EditPart) editPartSelection.get(i);
            if ((EObject) editpart.getModel() instanceof EObject)
                modelSelection.add((EObject) editpart.getModel());
        }
    }

}