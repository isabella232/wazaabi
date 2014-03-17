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

public class RemoveParameterCommand extends TransactionalEditingDomainCommand {

	private Parameter parameter = null;
	private Parameterized parameterized = null;

	private int index = -1;

	public RemoveParameterCommand() {
		super("Remove Parametert");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getParameterized() != null
				&& getParameter() != null;
	}

	@Override
	public boolean canUndo() {
		return super.canUndo() & index != -1;
	}

	@Override
	protected void doExecute() {
		index = getParameterized().getParameters().indexOf(getParameter());
		doRedo();
	}

	@Override
	protected void doRedo() {
		getParameterized().getParameters().remove(getParameter());
	}

	@Override
	protected void doUndo() {
		getParameterized().getParameters().add(index, getParameter());
	}

	public Parameter getParameter() {
		return parameter;
	}

	public Parameterized getParameterized() {
		return parameterized;
	}

	public void setParameter(Parameter parameter) {
		this.parameter = parameter;
	}

	public void setParameterized(Parameterized parameterized) {
		this.parameterized = parameterized;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(parameterized));
	}

}
