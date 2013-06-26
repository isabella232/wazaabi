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

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.ImageRuleManager;
import org.eclipse.wazaabi.mm.core.styles.BlankRule;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class SWTContainerView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView {

	private FormToolkit formToolkit = null;
	private Image image = null;

	public SWTContainerView(FormToolkit formToolkit) {
		this.formToolkit = formToolkit;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTContainerView() {
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
		} else if (ContainerEditPart.FORM_HEADER_IMAGE.equals(rule
				.getPropertyName())) {
			if (rule instanceof ImageRule)
				setFormHeaderImage((ImageRule) rule);
			else
				setFormHeaderImage(null);
		} else if (ContainerEditPart.FORM_DECORATE_FORM_HEADING.equals(rule
				.getPropertyName())) {
			if (rule instanceof BooleanRule)
				setDecorateFormHeading((BooleanRule) rule);
			else
				setDecorateFormHeading(null);
		}

		super.updateStyleRule(rule);
	}

	public void setFormHeaderTitle(StringRule rule) {
		if (rule != null)
			if (getSWTWidget() instanceof Form && !getSWTWidget().isDisposed())
				((Form) getSWTWidget()).setText(rule.getValue() != null ? rule
						.getValue() : ""); //$NON-NLS-1$

	}

	public void setDecorateFormHeading(BooleanRule rule) {
		if (rule != null)
			if (getSWTWidget() instanceof Form && !getSWTWidget().isDisposed())
				formToolkit.decorateFormHeading(((Form) getSWTWidget()));
	}

	@Override
	protected Composite createComposite(Composite parent, int style) {
		List<StyleRule> formSpecificRules = getFormSpecificRules();
		if (!formSpecificRules.isEmpty()) {
			formToolkit = new FormToolkit(parent.getDisplay());
			Form form = formToolkit.createForm((Composite) parent);
			return form;
		}
		if (formToolkit != null)
			return formToolkit.createComposite(parent, style);
		return super.createComposite(parent, style);
	}

	@Override
	protected Widget createExpandBar(Composite parent, int style) {
		if (formToolkit != null) {
			ExpandableComposite expandableComposite = formToolkit
					.createSection((org.eclipse.swt.widgets.Composite) parent,
							Section.DESCRIPTION | Section.TITLE_BAR
									| Section.TWISTIE | Section.EXPANDED);
			// ExpandableComposite expandableComposite = formToolkit
			// .createExpandableComposite(
			// (org.eclipse.swt.widgets.Composite) parent,
			// ExpandableComposite.TREE_NODE
			// | ExpandableComposite.CLIENT_INDENT);
			org.eclipse.swt.widgets.Composite content = formToolkit
					.createComposite(expandableComposite);
			content.setLayout(new FillLayout());
			expandableComposite.setClient(content);
			return expandableComposite;
		}
		return super.createExpandBar(parent, style);
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
			else if (ContainerEditPart.FORM_HEADER_IMAGE.equals(rule
					.getPropertyName())
					&& rule instanceof ImageRule
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
		if (image != null && !image.isDisposed())
			image.dispose();
		if (formToolkit != null)
			formToolkit.dispose();
	}

	@Override
	public Widget getContentPane() {
		if (getSWTWidget() instanceof Form)
			return ((Form) getSWTWidget()).getBody();
		if (getSWTWidget() instanceof ExpandableComposite)
			return ((ExpandableComposite) getSWTWidget()).getClient();
		return getSWTWidget();
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule styleRule, Widget widget) {
		if (styleRule == null)
			return false;
		if (ContainerEditPart.FORM_HEADER_TITLE.equals(styleRule
				.getPropertyName())) {
			if (styleRule instanceof StringRule)
				return !(getSWTWidget() instanceof Form);
			if (styleRule instanceof BlankRule)
				return getSWTWidget() instanceof Form;
			return false;
		}
		if (ContainerEditPart.FORM_HEADER_IMAGE.equals(styleRule
				.getPropertyName())) {
			if (styleRule instanceof ImageRule)
				return !(getSWTWidget() instanceof Form);
			if (styleRule instanceof BlankRule)
				return getSWTWidget() instanceof Form;
			return false;
		}
		if (ContainerEditPart.FORM_DECORATE_FORM_HEADING.equals(styleRule
				.getPropertyName())) {
			if (styleRule instanceof BooleanRule)
				return !(getSWTWidget() instanceof Form);
			if (styleRule instanceof BlankRule)
				return getSWTWidget() instanceof Form;
			return false;
		}

		return super.needReCreateWidgetView(styleRule, widget);
	}

	protected void setFormHeaderImage(ImageRule rule) {
		if (!(getSWTWidget() instanceof Form))
			return;
		if (rule == null)
			if (image == null)
				return;
			else {
				image.dispose();
				image = null;
			}
		else {
			Image newImage = ImageRuleManager.convertToPlatformSpecificObject(
					this, rule);
			if (image != null) {
				if (newImage != null
						&& image.getImageData().equals(newImage.getImageData()))
					return;
				image.dispose();
			}
			image = newImage;
		}
		((Form) getSWTWidget()).setImage(image);
		((Form) getSWTWidget()).update();
		revalidate();
	}
}
