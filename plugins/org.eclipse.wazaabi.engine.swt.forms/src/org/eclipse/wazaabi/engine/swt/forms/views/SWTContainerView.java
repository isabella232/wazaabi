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

import java.util.List;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.swt.forms.editparts.ContainerEditPart;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.ImageRuleManager;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SWTContainerView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTContainerView {

	private FormToolkit formToolkit = null;
	private Image image = null;
	private Composite innerComposite = null;
	private final SWTContainerView containingForm;

	private final Logger logger = LoggerFactory
			.getLogger(SWTContainerView.class);

	public static final String FORM_STYLE = "form"; //$NON-NLS-1$
	public static final String SECTION_STYLE = "section"; //$NON-NLS-1$

	public SWTContainerView(SWTContainerView containingForm) {
		this.containingForm = containingForm;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTContainerView() {
		this.containingForm = null;
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (ContainerEditPart.TITLE.equals(rule.getPropertyName())) {
			if (rule instanceof StringRule)
				setTitle((StringRule) rule);
			else
				setTitle(null);
		} else if (ContainerEditPart.HEADER_IMAGE
				.equals(rule.getPropertyName())) {
			if (rule instanceof ImageRule)
				setHeaderImage((ImageRule) rule);
			else
				setHeaderImage(null);
		} else if (ContainerEditPart.FORM_DECORATE_FORM_HEADING.equals(rule
				.getPropertyName())) {
			if (rule instanceof BooleanRule)
				setDecorateFormHeading((BooleanRule) rule);
			else
				setDecorateFormHeading(null);
		}

		super.updateStyleRule(rule);
	}

	public void setTitle(StringRule rule) {
		if (rule == null || getSWTWidget().isDisposed())
			return;
		if (getSWTWidget() instanceof Form)
			((Form) getSWTWidget()).setText(rule.getValue() != null ? rule
					.getValue() : ""); //$NON-NLS-1$
		else if (getSWTWidget() instanceof ExpandableComposite)
			((ExpandableComposite) getSWTWidget())
					.setText(rule.getValue() != null ? rule.getValue() : ""); //$NON-NLS-1$
		else
			super.setTitle(rule);
	}

	public void setDecorateFormHeading(BooleanRule rule) {
		if (rule != null)
			if (getSWTWidget() instanceof Form && !getSWTWidget().isDisposed())
				getFormToolkit().decorateFormHeading(((Form) getSWTWidget()));
	}

	@Override
	protected Composite createComposite(Composite parent, int style) {

		StringRule lafRule = (StringRule) ((StyledElement) getHost().getModel())
				.getFirstStyleRule(AbstractComponentEditPart.LOOK_AND_FEEL,
						CoreStylesPackage.Literals.STRING_RULE);
		if (lafRule != null) {
			String laf = lafRule.getValue();
			if (SECTION_STYLE.equals(laf) && getFormToolkit() != null) {
				Section section = getFormToolkit().createSection(
						(org.eclipse.swt.widgets.Composite) parent,
						/*
						 * Section.DESCRIPTION | Section.TITLE_BAR |
						 */Section.TREE_NODE | Section.EXPANDED);
				innerComposite = getFormToolkit().createComposite(section);
				innerComposite.setLayout(new FillLayout());
				section.setClient(innerComposite);
				return section;
			}
			if ("expandedComposite".equals(laf) && getFormToolkit() != null) {

			}
			if (FORM_STYLE.equals(laf)) {
				if (containingForm == null || formToolkit == null)
					formToolkit = new FormToolkit(parent.getDisplay());
				if (getFormToolkit() != null)
					return getFormToolkit().createForm((Composite) parent);
			}

		}
		if (getFormToolkit() != null)
			return getFormToolkit().createComposite(parent, style);
		return super.createComposite(parent, style);
	}

	@Override
	protected Widget createExpandBar(Composite parent, int style) {
		if (getFormToolkit() != null) {
			ExpandableComposite expandableComposite = getFormToolkit()
					.createSection(
							(org.eclipse.swt.widgets.Composite) parent,
							Section.DESCRIPTION | Section.TITLE_BAR
									| Section.TWISTIE | Section.EXPANDED);
			// ExpandableComposite expandableComposite = formToolkit
			// .createExpandableComposite(
			// (org.eclipse.swt.widgets.Composite) parent,
			// ExpandableComposite.TREE_NODE
			// | ExpandableComposite.CLIENT_INDENT);
			org.eclipse.swt.widgets.Composite content = getFormToolkit()
					.createComposite(expandableComposite);
			content.setLayout(new FillLayout());
			expandableComposite.setClient(content);
			return expandableComposite;
		}
		return super.createExpandBar(parent, style);
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

	public FormToolkit getFormToolkit() {
		if (formToolkit == null && containingForm != null)
			formToolkit = containingForm.getFormToolkit();
		return formToolkit;
	}

	@Override
	protected void widgetDisposed() {
		super.widgetDisposed();
		if (innerComposite != null && !innerComposite.isDisposed()) {
			innerComposite.dispose();
			innerComposite = null;
		}
		if (image != null && !image.isDisposed()) {
			image.dispose();
			image = null;
		}
		if (containingForm == null && formToolkit != null) {
			formToolkit.dispose();
			formToolkit = null;
		}
	}

	@Override
	public Widget getContentPane() {
		if (getSWTWidget() instanceof Form)
			return ((Form) getSWTWidget()).getBody();
		if (getSWTWidget() instanceof ExpandableComposite)
			if (innerComposite != null && !innerComposite.isDisposed())
				return innerComposite;
			else
				logger.error("Section or ExpandableComposite without innerComposite"); //$NON-NLS-1$
		return getSWTWidget();
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule styleRule, Widget widget) {
		if (styleRule == null)
			return false;

		// if (ContainerEditPart.FORM_HEADER_TITLE.equals(styleRule
		// .getPropertyName())) {
		// if (styleRule instanceof StringRule)
		// return !(getSWTWidget() instanceof Form);
		// if (styleRule instanceof BlankRule)
		// return getSWTWidget() instanceof Form;
		// return false;
		// }
		// if (ContainerEditPart.FORM_HEADER_IMAGE.equals(styleRule
		// .getPropertyName())) {
		// if (styleRule instanceof ImageRule)
		// return !(getSWTWidget() instanceof Form);
		// if (styleRule instanceof BlankRule)
		// return getSWTWidget() instanceof Form;
		// return false;
		// }
		// if (ContainerEditPart.FORM_DECORATE_FORM_HEADING.equals(styleRule
		// .getPropertyName())) {
		// if (styleRule instanceof BooleanRule)
		// return !(getSWTWidget() instanceof Form);
		// if (styleRule instanceof BlankRule)
		// return getSWTWidget() instanceof Form;
		// return false;
		// }

		return super.needReCreateWidgetView(styleRule, widget);
	}

	@Override
	protected boolean isWidgetWithoutLookAndFeel(Widget widget) {
		// unfortunately, LayoutComposite which is the Composite created by
		// FormToolkit.createComposite(...) is not visible from outside its
		// package
		return widget != null
				&& widget.getClass().getName()
						.equals("org.eclipse.ui.forms.widgets.LayoutComposite"); //$NON-NLS-1$
	}

	@Override
	protected boolean matchLookAndFeel(String laf, Widget widget) {
		if (FORM_STYLE.equals(laf))
			return widget instanceof Form;
		if (SECTION_STYLE.equals(laf))
			return widget instanceof Section;
		return super.matchLookAndFeel(laf, widget);
	}

	protected void setHeaderImage(ImageRule rule) {
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
