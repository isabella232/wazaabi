/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editparts.commands.components;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;

public class SetRootInResourceCommand extends TransactionalEditingDomainCommand {

	private AbstractComponent child;
	private List<EObject> previousContents = null;
	private Resource resource;

	public SetRootInResourceCommand() {
		super("SetRootInResourceCommand"); // TODO : localize that
	}

	public boolean canExecute() {
		return getChild() != null && getResource() != null
				&& getResource().getContents().isEmpty() && super.canExecute();
	}

	@Override
	protected void doExecute() {
		doRedo();
	}

	@Override
	protected void doRedo() {
		if (!getResource().getContents().isEmpty()) {
			previousContents = new ArrayList<EObject>();
			for (EObject object : getResource().getContents())
				previousContents.add(object);
			getResource().getContents().clear();
		}
		getResource().getContents().add(getChild());
	}

	public void setChild(AbstractComponent child) {
		this.child = child;
	}

	public void setResource(Resource resource) {
		this.resource = resource;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(resource));
	}

	@Override
	protected void doUndo() {
		getResource().getContents().remove(getChild());
		if (previousContents != null) {
			getResource().getContents().addAll(previousContents);
			previousContents.clear();
		}
	}

	public AbstractComponent getChild() {
		return child;
	}

	public Resource getResource() {
		return resource;
	}

}
