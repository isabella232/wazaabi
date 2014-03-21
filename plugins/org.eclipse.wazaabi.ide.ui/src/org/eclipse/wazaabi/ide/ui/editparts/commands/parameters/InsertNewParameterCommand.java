/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editparts.commands.parameters;

import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;

public class InsertNewParameterCommand extends
		TransactionalEditingDomainCommand {

	private Parameter newParameter = null;
	private Parameterized parameterized = null;

	private int index = -1;

	public InsertNewParameterCommand() {
		super("Insert New Parameter");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getParameterized() != null
				&& getParameter() != null && getIndex() >= 0
				|| getIndex() < getParameterized().getParameters().size();
	}

	@Override
	public boolean canUndo() {
		return super.canUndo();
	}

	@Override
	protected void doExecute() {
		doRedo();
	}

	@Override
	protected void doRedo() {
		if (getIndex() == -1)
			getParameterized().getParameters().add(getParameter());
		else
			getParameterized().getParameters().add(getIndex(), getParameter());
	}

	@Override
	protected void doUndo() {
		getParameterized().getParameters().remove(getParameter());
	}

	public int getIndex() {
		return index;
	}

	public Parameter getParameter() {
		return newParameter;
	}

	public Parameterized getParameterized() {
		return parameterized;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setNewParameter(Parameter newParameter) {
		this.newParameter = newParameter;
	}

	public void setParameterized(Parameterized parameterized) {
		this.parameterized = parameterized;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(parameterized));
	}

}
