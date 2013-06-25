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

package org.eclipse.wazaabi.engine.swt.forms.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class SWTContainerView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView {

	private FormToolkit formToolkit = null;

	public SWTContainerView(FormToolkit formToolkit) {
		this.formToolkit = formToolkit;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTContainerView() {
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		if (formToolkit != null)
			return createComposite(parent);
		List<StyleRule> formSpecificRules = getFormSpecificRules();
		if (formSpecificRules.isEmpty())
			return createComposite(parent);

		formToolkit = new FormToolkit(parent.getDisplay());
		Form form = formToolkit.createForm((Composite) parent);
		return form;
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (ContainerEditPart.FORM_HEADER_TITLE.equals(rule.getPropertyName())) {
			if (rule instanceof StringRule)
				setFormHeaderTitle((StringRule) rule);
			else
				setFormHeaderTitle(null);
		} else
			super.updateStyleRule(rule);
	}

	public void setFormHeaderTitle(StringRule rule) {
		if (rule != null)
			if (getSWTWidget() instanceof Form && !getSWTWidget().isDisposed())
				((Form) getSWTWidget()).setText(rule.getValue() != null ? rule
						.getValue() : ""); //$NON-NLS-1$
	}

	protected org.eclipse.swt.widgets.Widget createComposite(Widget parent) {
		StringRule containerTitleRule = (StringRule) ((StyledElement) getHost()
				.getModel()).getFirstStyleRule(
				ContainerEditPart.TITLE_VALUE_PROPERTY_NAME, null);
		BooleanRule containerBorderRule = (BooleanRule) ((StyledElement) getHost()
				.getModel()).getFirstStyleRule(
				ContainerEditPart.TITLE_BORDER_PROPERTY_NAME, null);
		Composite composite;
		if (containerTitleRule != null && containerBorderRule != null
				&& !containerTitleRule.getValue().equalsIgnoreCase("")) {
			composite = new org.eclipse.swt.widgets.Group((Composite) parent,
					computeSWTCreationStyle(getHost()));
			((Group) composite).setText(containerTitleRule.getValue());
		} else {
			if (formToolkit != null)
				composite = formToolkit.createComposite((Composite) parent,
						computeSWTCreationStyle(getHost()));
			else
				composite = new Composite((Composite) parent,
						computeSWTCreationStyle(getHost()));
		}
		return wrapForSpecificParent((Composite) parent, composite);
	}

	protected List<StyleRule> getFormSpecificRules() {
		List<StyleRule> rules = new ArrayList<StyleRule>();
		for (StyleRule rule : ((StyledElement) getHost().getModel())
				.getStyleRules()) {
			if (ContainerEditPart.FORM_HEADER_TITLE.equals(rule
					.getPropertyName())
					&& rule instanceof StringRule
					&& !containsRule(rules, rule))
				rules.add(rule);
		}
		return rules;
	}

	/**
	 * Returns true if the an item or rules has the same property name than
	 * rule.
	 * 
	 * @param rules
	 *            A List of StyleRules
	 * @param rule
	 *            The rule whose property name is compared to.
	 * @return
	 */
	protected boolean containsRule(List<StyleRule> rules, StyleRule rule) {
		if (rules == null || rule == null || rule.getPropertyName() == null)
			return false;
		for (StyleRule item : rules)
			if (rule.getPropertyName().equals(item.getPropertyName()))
				return true;
		return false;
	}

	protected FormToolkit getFormToolkit() {
		return formToolkit;
	}

	@Override
	protected void widgetDisposed() {
		super.widgetDisposed();
		if (formToolkit != null)
			formToolkit.dispose();
	}

	@Override
	public Widget getContentPane() {
		if (getSWTWidget() instanceof Form)
			return ((Form) getSWTWidget()).getBody();
		return getSWTWidget();
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule styleRule, Widget widget) {
		if (styleRule == null)
			return false;
		if (ContainerEditPart.FORM_HEADER_TITLE.equals(styleRule
				.getPropertyName()) && !(getSWTWidget() instanceof Form))
			return true;
		return super.needReCreateWidgetView(styleRule, widget);
	}

}
