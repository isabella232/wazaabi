/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
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

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gef.ui.actions.SelectionAction;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.wazaabi.ide.ui.editors.TreeEditorMessages;
import org.eclipse.wazaabi.ide.ui.editors.actions.wizards.SelectECoreElementWizard;
import org.eclipse.wazaabi.ide.ui.editpolicies.InsertTransformedMetamodelElementRequest;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class InsertECoreElementAction extends SelectionAction {

	public static final String ID = "InsertECoreElementAction"; //$NON-NLS-1$

	InsertTransformedMetamodelElementRequest request;

	public InsertECoreElementAction(IWorkbenchPart part) {
		super(part);
		request = new InsertTransformedMetamodelElementRequest();
		setText(TreeEditorMessages.InsertECoreElementAction_ActionLabelText);
		setId(ID);
		setToolTipText(TreeEditorMessages.InsertECoreElementAction_ActionToolTipText);
		// setImageDescriptor(ImageDescriptor.createFromFile(Activator.class,
		//				"icons/plus.gif")); //$NON-NLS-1$

		setHoverImageDescriptor(getImageDescriptor());
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

	private Command getCommand(EObject metamodelElement) {
		@SuppressWarnings("unchecked")
		List<Object> editparts = getSelectedObjects();
		CompoundCommand cc = new CompoundCommand();
		cc.setDebugLabel("Insert ECore Element");//$NON-NLS-1$
		for (int i = 0; i < editparts.size(); i++) {
			EditPart part = (EditPart) editparts.get(i);
			request.setMetamodelElement(metamodelElement);
			cc.add(part.getCommand(request));
		}
		return cc;
	}

	public void run() {
		SelectECoreElementWizard wizard = new SelectECoreElementWizard();
		WizardDialog dialog = new WizardDialog(getWorkbenchPart().getSite()
				.getShell(), wizard);
		if (dialog.open() == WizardDialog.OK) {
			execute(getCommand(wizard.getResult()));
		}

	}

}
