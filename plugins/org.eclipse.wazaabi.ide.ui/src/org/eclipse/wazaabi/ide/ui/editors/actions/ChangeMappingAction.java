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

package org.eclipse.wazaabi.ide.ui.editors.actions;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.ui.actions.SelectionAction;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.wazaabi.ide.mapping.rules.MappingMethodDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.LabelProviderInfo;
import org.eclipse.wazaabi.ide.ui.editpolicies.InsertTransformedMetamodelElementRequest;
import org.eclipse.wazaabi.ide.ui.propertysheetpage.EventHandlerLocator;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class ChangeMappingAction extends SelectionAction {

	public static final String ID = "ChangeMappingAction"; //$NON-NLS-1$

	InsertTransformedMetamodelElementRequest request;

	public ChangeMappingAction(IWorkbenchPart part) {
		super(part);
		// request = new InsertTransformedMetamodelElementRequest();
		setText("Change Mapping...");
		setId(ID);
		// setToolTipText(TreeEditorMessages.InsertECoreElementAction_ActionToolTipText);
		// // setImageDescriptor(ImageDescriptor.createFromFile(Activator.class,
		//        //                "icons/plus.gif")); //$NON-NLS-1$
		//
		// setHoverImageDescriptor(getImageDescriptor());
	}

	protected boolean calculateEnabled() {
		return canPerformAction();
	}

	private boolean canPerformAction() {
		if (getSelectedObjects().isEmpty())
			return false;
		@SuppressWarnings("unchecked")
		List<Object> parts = getSelectedObjects();
		for (int i = 0; i < parts.size(); i++) {
			Object o = parts.get(i);
			if (!(o instanceof EditPart))
				return false;
			EditPart part = (EditPart) o;
			if (!(part.getModel() instanceof Container))
				return false;
		}
		return true;
	}

	private List<EObject> getSelectedEObject() {
		if (getSelectedObjects().isEmpty())
			return Collections.emptyList();

		List<EObject> selectedEObjects = new ArrayList<EObject>();
		@SuppressWarnings("unchecked")
		List<Object> parts = getSelectedObjects();
		for (int i = 0; i < parts.size(); i++) {
			Object o = parts.get(i);
			if (!(o instanceof EditPart))
				continue;
			EditPart part = (EditPart) o;
			if (part.getModel() instanceof EObject)
				selectedEObjects.add(((EObject) part.getModel()));
		}
		return selectedEObjects;
	}

	// private Command getCommand(EObject metamodelElement) {
	// @SuppressWarnings("unchecked")
	// List<Object> editparts = getSelectedObjects();
	// CompoundCommand cc = new CompoundCommand();
	//        cc.setDebugLabel("Insert ECore Element");//$NON-NLS-1$
	// for (int i = 0; i < editparts.size(); i++) {
	// EditPart part = (EditPart) editparts.get(i);
	// request.setMetamodelElement(metamodelElement);
	// cc.add(part.getCommand(request));
	// }
	// return cc;
	// }

	public void run() {
		List<EObject> selectedEObjects = getSelectedEObject();
		if (selectedEObjects.isEmpty())
			return;

		ElementListSelectionDialog dialog = new ElementListSelectionDialog(
				getWorkbenchPart().getSite().getShell(), new ILabelProvider() {

					public void removeListener(ILabelProviderListener listener) {
						// TODO Auto-generated method stub

					}

					public boolean isLabelProperty(Object element,
							String property) {
						// TODO Auto-generated method stub
						return false;
					}

					public void dispose() {
						// TODO Auto-generated method stub

					}

					public void addListener(ILabelProviderListener listener) {
						// TODO Auto-generated method stub

					}

					public String getText(Object element) {
						if (element instanceof MappingMethodDescriptor)
							return getLabelProviderInfoText((MappingMethodDescriptor) element);
						return element != null ? element.toString() : "";
					}

					public Image getImage(Object element) {
						// TODO Auto-generated method stub
						return null;
					}
				});

		EventHandlerLocator locator = new EventHandlerLocator();
		dialog.setElements(locator.getURIs("execute", 3).toArray());

		// dialog.setElements(IDESingleton
		// .getMappingRuleManager()
		// .getDescriptors(selectedEObjects.get(0),
		// EcorePackage.Literals.EENUM,
		// CoreWidgetsPackage.Literals.ABSTRACT_COMPONENT)
		// .toArray());
		dialog.open();

		// SelectECoreElementWizard wizard = new SelectECoreElementWizard();
		//
		//
		//
		// WizardDialog dialog = new WizardDialog(getWorkbenchPart().getSite()
		// .getShell(), wizard);
		// if (dialog.open() == WizardDialog.OK) {
		// execute(getCommand(wizard.getResult()));
		// }

	}

	protected String getLabelProviderInfoText(MappingMethodDescriptor descriptor) {
		if (descriptor != null) {
			Method method = descriptor.getMethod();
			if (method != null) {
				LabelProviderInfo labelProviderInfo = method
						.getAnnotation(LabelProviderInfo.class);
				if (labelProviderInfo != null)
					return labelProviderInfo.text() != null ? labelProviderInfo
							.text() : "";
			}
		}
		return "";
	}

}