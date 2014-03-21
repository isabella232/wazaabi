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

package org.eclipse.wazaabi.ide.ui.editparts.commands.stylerules;

import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class RemoveStyleRuleCommand extends TransactionalEditingDomainCommand {

	private StyleRule styleRule = null;
	private StyledElement styledElement = null;

	private int index = -1;

	public RemoveStyleRuleCommand() {
		super("Remove StyleRule");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getStyledElement() != null
				&& getStyleRule() != null;
	}

	@Override
	public boolean canUndo() {
		return super.canUndo() & index != -1;
	}

	@Override
	protected void doExecute() {
		index = getStyledElement().getStyleRules().indexOf(getStyleRule());
		doRedo();
	}

	@Override
	protected void doRedo() {
		getStyledElement().getStyleRules().remove(getStyleRule());
	}

	@Override
	protected void doUndo() {
		getStyledElement().getStyleRules().add(index, getStyleRule());
	}

	public StyleRule getStyleRule() {
		return styleRule;
	}

	public StyledElement getStyledElement() {
		return styledElement;
	}

	public void setStyleRule(StyleRule styleRule) {
		this.styleRule = styleRule;
	}

	public void setStyledElement(StyledElement styledElement) {
		this.styledElement = styledElement;
		setTransactionalEditingDomain(CommandsUtils
				.getEditingDomain(styledElement));
	}

}
