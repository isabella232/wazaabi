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

import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;

public abstract class AbstractSetUniqueStyleRuleCommand extends
		TransactionalEditingDomainCommand {

	private String propertyName;
	private Widget widget;
	private Object newValue;
	private List<RuleEntry> previousRuleEntries = new ArrayList<RuleEntry>();
	private StyleRule modifiedStyleRule = null;

	private class RuleEntry {
		private final int index;
		private final StyleRule styleRule;

		public RuleEntry(int index, StyleRule styleRule) {
			this.index = index;
			this.styleRule = (StyleRule) EcoreUtil.copy(styleRule);
		}

		public int getIndex() {
			return index;
		}

		public StyleRule getStyleRule() {
			return styleRule;
		}

	};

	protected void gatherPreviousRuleEntries() {
		for (int i = 0; i < getWidget().getStyleRules().size(); i++)
			if (getPropertyName().equals(
					getWidget().getStyleRules().get(i).getPropertyName()))
				previousRuleEntries.add(new RuleEntry(i, getWidget()
						.getStyleRules().get(i)));
	}

	protected void removeUnusedStyleRules() {
		if (previousRuleEntries.size() < 2)
			return;
		// remove all the rules except the first one
		for (int i = previousRuleEntries.size() - 1; i > 0; i--)
			getWidget().getStyleRules().remove(
					previousRuleEntries.get(i).getIndex());
	}

	@Override
	protected void doExecute() {
		gatherPreviousRuleEntries();
		removeUnusedStyleRules();
		if (previousRuleEntries.size() == 0) {
			modifiedStyleRule = createNewStyleRule();
			modifiedStyleRule.setPropertyName(getPropertyName());
		} else
			modifiedStyleRule = getWidget().getStyleRules().get(
					previousRuleEntries.get(0).getIndex());

		doRedo();
	}

	@Override
	protected void doRedo() {
		setStyleRuleValue(modifiedStyleRule, getNewValue());
		if (previousRuleEntries.size() == 0)
			getWidget().getStyleRules().add(modifiedStyleRule);
	}

	@Override
	protected void doUndo() {
		if (previousRuleEntries.size() == 0)
			getWidget().getStyleRules().remove(modifiedStyleRule);
		else {
			setStyleRuleValue(
					modifiedStyleRule,
					getStyleRuleValue(previousRuleEntries.get(0).getStyleRule()));
			for (int i = 1; i < previousRuleEntries.size(); i++)
				getWidget().getStyleRules().add(
						previousRuleEntries.get(i).getIndex(),
						previousRuleEntries.get(i).getStyleRule());
		}
	}

	public String getPropertyName() {
		return propertyName;
	}

	public void setPropertyName(String propertyName) {
		this.propertyName = propertyName;
	}

	public Widget getWidget() {
		return widget;
	}

	public void setWidget(Widget widget) {
		this.widget = widget;
	}

	public Object getNewValue() {
		return newValue;
	}

	public void setNewValue(Object newValue) {
		this.newValue = newValue;
	}

	protected abstract StyleRule createNewStyleRule();

	protected abstract void setStyleRuleValue(StyleRule rule, Object newValue);

	protected abstract Object getStyleRuleValue(StyleRule rule);

	@Override
	public boolean canExecute() {
		return super.canExecute() && getPropertyName() != null
				&& !"".equals(getPropertyName()) && getWidget() != null; //$NON-NLS-1$
	}

	@Override
	public boolean canUndo() {
		return super.canUndo() && modifiedStyleRule != null;
	}

}
