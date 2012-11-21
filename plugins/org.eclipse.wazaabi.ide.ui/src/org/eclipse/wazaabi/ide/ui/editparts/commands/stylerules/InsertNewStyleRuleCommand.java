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

package org.eclipse.wazaabi.ide.ui.editparts.commands.stylerules;

import org.eclipse.wazaabi.ide.ui.editparts.commands.CommandsUtils;
import org.eclipse.wazaabi.ide.ui.editparts.commands.TransactionalEditingDomainCommand;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class InsertNewStyleRuleCommand extends
		TransactionalEditingDomainCommand {

	private StyleRule newStyleRule = null;
	private StyledElement styledElement = null;

	private int index = -1;

	public InsertNewStyleRuleCommand() {
		super("Insert New StyleRule");
	}

	@Override
	public boolean canExecute() {
		return super.canExecute() && getStyledElement() != null
				&& getNewStyleRule() != null && getIndex() >= 0
				|| getIndex() < getStyledElement().getStyleRules().size();
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
		getStyledElement().getStyleRules().add(getIndex(), getNewStyleRule());
	}

	@Override
	protected void doUndo() {
		getStyledElement().getStyleRules().remove(getNewStyleRule());
	}

	public int getIndex() {
		return index;
	}

	public StyleRule getNewStyleRule() {
		return newStyleRule;
	}

	public StyledElement getStyledElement() {
		return styledElement;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setNewStyleRule(StyleRule newStyleRule) {
		this.newStyleRule = newStyleRule;
	}

	public void setStyledElement(StyledElement styledElement) {
		this.styledElement = styledElement;
		setTransactionalEditingDomain(CommandsUtils.getEditingDomain(styledElement));
	}

}
