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

package org.eclipse.wazaabi.ide.ui.editparts.commands;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class InsertNewUniqueLayoutCommand extends
		TransactionalEditingDomainCommand {

	private LayoutRule layout;
	private List<LayoutRule> previousLayouts = new ArrayList<LayoutRule>();
	private Container container;

	public InsertNewUniqueLayoutCommand() {
		super("InsertNewLayoutCommand"); // TODO : localize that
	}

	public boolean canExecute() {
		return getLayout() != null && getContainer() != null
				&& super.canExecute();
	}

	@Override
	protected void doExecute() {
		for (StyleRule rule : getContainer().getStyleRules())
			if (rule instanceof LayoutRule)
				previousLayouts.add((LayoutRule) rule);
		doRedo();
	}

	@Override
	protected void doRedo() {
		for (LayoutRule rule : previousLayouts)
			getContainer().getStyleRules().remove(rule);
		getContainer().getStyleRules().add(getLayout());
		getLayout().setPropertyName("layout");
	}

	public void setLayout(LayoutRule layout) {
		this.layout = layout;
	}

	public void setContainer(Container container) {
		this.container = container;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(container));
	}

	@Override
	protected void doUndo() {
		getContainer().getStyleRules().remove(getLayout());
		for (LayoutRule rule : previousLayouts)
			getContainer().getStyleRules().add(rule);
	}

	public LayoutRule getLayout() {
		return layout;
	}

	public Container getContainer() {
		return container;
	}

}
