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

import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;

public class InsertNewUniqueLayoutDataCommand extends
		TransactionalEditingDomainCommand {

	private LayoutDataRule layoutData;
	private List<LayoutDataRule> previousLayoutData = new ArrayList<LayoutDataRule>();
	private AbstractComponent component;

	public InsertNewUniqueLayoutDataCommand() {
		super("InsertNewLayoutDataCommand"); // TODO : localize that
	}

	public boolean canExecute() {
		return getLayoutData() != null && getComponent() != null
				&& super.canExecute();
	}

	@Override
	protected void doExecute() {
		for (StyleRule rule : getComponent().getStyleRules())
			if (rule instanceof LayoutDataRule)
				previousLayoutData.add((LayoutDataRule) rule);
		doRedo();
	}

	@Override
	protected void doRedo() {
		for (LayoutDataRule rule : previousLayoutData)
			getComponent().getStyleRules().remove(rule);
		getComponent().getStyleRules().add(getLayoutData());
		getLayoutData().setPropertyName("layout-data");
	}

	public void setLayoutData(LayoutDataRule layoutData) {
		this.layoutData = layoutData;
	}

	public void setComponent(AbstractComponent component) {
		this.component = component;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(component));
	}

	@Override
	protected void doUndo() {
		getComponent().getStyleRules().remove(getLayoutData());
		for (LayoutDataRule rule : previousLayoutData)
			getComponent().getStyleRules().add(rule);
	}

	public LayoutDataRule getLayoutData() {
		return layoutData;
	}

	public AbstractComponent getComponent() {
		return component;
	}

}
