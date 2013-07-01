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
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.swt.commons.editparts.stylerules.managers.ImageRuleManager;
import org.eclipse.wazaabi.engine.swt.forms.editparts.ContainerEditPart;
import org.eclipse.wazaabi.mm.core.styles.BlankRule;
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
	public static final String TWISTIE_STYLE = "twistie"; //$NON-NLS-1$
	public static final String TREE_NODE_STYLE = "tree-node"; //$NON-NLS-1$

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
		} else if (ContainerEditPart.DECORATE_FORM_HEADING.equals(rule
				.getPropertyName())) {
			if (rule instanceof BooleanRule)
				setDecorateFormHeading((BooleanRule) rule);
			else
				setDecorateFormHeading(null);
		} else if (ContainerEditPart.DESCRIPTION.equals(rule.getPropertyName())) {
			if (rule instanceof StringRule)
				setDescription((StringRule) rule);
			else
				setDescription(null);
		} else if (ContainerEditPart.EXPANDED.equals(rule.getPropertyName())) {
			if (rule instanceof BooleanRule)
				setExpanded((BooleanRule) rule);
			else
				setExpanded(null);
		} else
			super.updateStyleRule(rule);
	}

	public void setExpanded(BooleanRule rule) {
		if (rule == null || getSWTWidget().isDisposed())
			return;
		if (getSWTWidget() instanceof Section)
			((Section) getSWTWidget()).setExpanded(rule.isValue());
	}

	public void setDescription(StringRule rule) {
		if (rule == null || getSWTWidget().isDisposed())
			return;
		if (getSWTWidget() instanceof Section)
			((Section) getSWTWidget())
					.setDescription(rule.getValue() != null ? rule.getValue()
							: ""); //$NON-NLS-1$
	}

	public void setTitle(StringRule rule) {
		if (rule == null || getSWTWidget().isDisposed())
			return;
		if (getSWTWidget() instanceof Form)
			((Form) getSWTWidget()).setText(rule.getValue() != null ? rule
					.getValue() : ""); //$NON-NLS-1$
		else if (getSWTWidget() instanceof Section)
			((Section) getSWTWidget()).setText(rule.getValue() != null ? rule
					.getValue() : ""); //$NON-NLS-1$
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
			if (SECTION_STYLE.equals(laf) && getFormToolkit() != null)
				return createSection(parent, style);
			if (FORM_STYLE.equals(laf)) {
				if (containingForm == null || formToolkit == null)
					formToolkit = new FormToolkit(parent.getDisplay());
				if (getFormToolkit() != null)
					return getFormToolkit().createForm(parent);
			}
		}
		if (getFormToolkit() != null)
			return getFormToolkit().createComposite(parent, style);
		return super.createComposite(parent, style);
	}

	protected Section createSection(Composite parent, int style) {
		StyledElement model = ((StyledElement) getHost().getModel());
		int expansionStyle = 0;

		// TODO : too much 'getFirstStyleRule(...)' calls which means too much
		// loops. Need to be improved

		StringRule expansionToggle = (StringRule) model.getFirstStyleRule(
				ContainerEditPart.EXPANSION_TOGGLE,
				CoreStylesPackage.Literals.STRING_RULE);
		if (expansionToggle != null) {
			if (TREE_NODE_STYLE.equals(expansionToggle.getValue()))
				expansionStyle |= Section.TREE_NODE;
			else if (TWISTIE_STYLE.equals(expansionToggle.getValue()))
				expansionStyle |= Section.TWISTIE;
		}

		StringRule description = (StringRule) model.getFirstStyleRule(
				ContainerEditPart.DESCRIPTION,
				CoreStylesPackage.Literals.STRING_RULE);
		if (description != null)
			expansionStyle |= Section.DESCRIPTION;

		StringRule title = (StringRule) model
				.getFirstStyleRule(ContainerEditPart.TITLE,
						CoreStylesPackage.Literals.STRING_RULE);
		if (title == null)
			expansionStyle |= Section.NO_TITLE;

		BooleanRule booleanRule = (BooleanRule) model.getFirstStyleRule(
				ContainerEditPart.TITLE_BAR,
				CoreStylesPackage.Literals.BOOLEAN_RULE);
		if (booleanRule != null && booleanRule.isValue())
			expansionStyle |= Section.TITLE_BAR;

		booleanRule = (BooleanRule) model.getFirstStyleRule(
				ContainerEditPart.SHORT_TITLE_BAR,
				CoreStylesPackage.Literals.BOOLEAN_RULE);
		if (booleanRule != null && booleanRule.isValue())
			expansionStyle |= Section.SHORT_TITLE_BAR;

		booleanRule = (BooleanRule) model.getFirstStyleRule(
				ContainerEditPart.CLIENT_INDENT,
				CoreStylesPackage.Literals.BOOLEAN_RULE);
		if (booleanRule != null && booleanRule.isValue())
			expansionStyle |= Section.CLIENT_INDENT;

		booleanRule = (BooleanRule) model.getFirstStyleRule(
				ContainerEditPart.COMPACT,
				CoreStylesPackage.Literals.BOOLEAN_RULE);
		if (booleanRule != null && booleanRule.isValue())
			expansionStyle |= Section.COMPACT;

		Section section = getFormToolkit().createSection(
				(org.eclipse.swt.widgets.Composite) parent, expansionStyle);

		// Since description is a 're-creation' style, we need to set it
		if (description != null)
			section.setDescription(description.getValue() != null ? description
					.getValue() : ""); //$NON-NLS-1$

		innerComposite = getFormToolkit().createComposite(section);
		innerComposite.setLayout(new FillLayout());
		section.setClient(innerComposite);
		return section;
	}

	@Override
	protected Widget createExpandBar(Composite parent, int style) {
		if (getFormToolkit() != null)
			return createSection(parent, style);
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
		if (getSWTWidget() instanceof Section)
			if (innerComposite != null && !innerComposite.isDisposed())
				return innerComposite;
			else
				logger.error("Section without innerComposite"); //$NON-NLS-1$
		return getSWTWidget();
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule styleRule, Widget widget) {
		if (styleRule == null)
			return false;
		if (widget instanceof Section) {

			if (ContainerEditPart.TITLE_BAR.equals(styleRule.getPropertyName())) {
				if (styleRule instanceof BooleanRule)
					return (((Section) widget).getExpansionStyle() & Section.TITLE_BAR) == 0;
				else if (styleRule instanceof BlankRule)
					return (((Section) widget).getExpansionStyle() & Section.TITLE_BAR) != 0;
				else
					return false;
			} else if (ContainerEditPart.SHORT_TITLE_BAR.equals(styleRule
					.getPropertyName())) {
				if (styleRule instanceof BooleanRule)
					return (((Section) widget).getExpansionStyle() & Section.SHORT_TITLE_BAR) == 0;
				else if (styleRule instanceof BlankRule)
					return (((Section) widget).getExpansionStyle() & Section.SHORT_TITLE_BAR) != 0;
				else
					return false;
			}
			if (ContainerEditPart.CLIENT_INDENT.equals(styleRule
					.getPropertyName())) {
				if (styleRule instanceof BooleanRule)
					return (((Section) widget).getExpansionStyle() & Section.CLIENT_INDENT) == 0;
				else if (styleRule instanceof BlankRule)
					return (((Section) widget).getExpansionStyle() & Section.CLIENT_INDENT) != 0;
				else
					return false;
			}
			if (ContainerEditPart.COMPACT.equals(styleRule.getPropertyName())) {
				if (styleRule instanceof BooleanRule)
					return (((Section) widget).getExpansionStyle() & Section.COMPACT) == 0;
				else if (styleRule instanceof BlankRule)
					return (((Section) widget).getExpansionStyle() & Section.COMPACT) != 0;
				else
					return false;
			} else if (ContainerEditPart.EXPANSION_TOGGLE.equals(styleRule
					.getPropertyName())) {
				if (styleRule instanceof StringRule)
					return !(isExpansionStyleBitCorrectlySet((Section) widget,
							Section.TREE_NODE,
							TREE_NODE_STYLE.equals(((StringRule) styleRule)
									.getValue())) & isExpansionStyleBitCorrectlySet(
							(Section) widget, Section.TWISTIE,
							TWISTIE_STYLE.equals(((StringRule) styleRule)
									.getValue())));
				if (styleRule instanceof BlankRule)
					return ((((Section) widget).getExpansionStyle() & Section.TREE_NODE) != 0)
							| ((((Section) widget).getExpansionStyle() & Section.TWISTIE) != 0);
				return false;
			} else if (ContainerEditPart.DESCRIPTION.equals(styleRule
					.getPropertyName())) {
				if (styleRule instanceof StringRule)
					return !isExpansionStyleBitCorrectlySet((Section) widget,
							Section.DESCRIPTION,
							((StringRule) styleRule).getValue() != null);
				else if (styleRule instanceof BlankRule)
					return (((Section) widget).getExpansionStyle() & Section.DESCRIPTION) != 0;
				return false;
			} else if (ContainerEditPart.TITLE.equals(styleRule
					.getPropertyName())) {
				if (styleRule instanceof StringRule)
					return (((Section) widget).getExpansionStyle() & Section.NO_TITLE) != 0;
				else if (styleRule instanceof BlankRule)
					return (((Section) widget).getExpansionStyle() & Section.NO_TITLE) == 0;
				return false;
			}
		} else if (widget instanceof Form) {
			if (ContainerEditPart.DECORATE_FORM_HEADING.equals(styleRule
					.getPropertyName())) {
				if (styleRule instanceof BooleanRule)
					return !(getSWTWidget() instanceof Form);
				if (styleRule instanceof BlankRule)
					return getSWTWidget() instanceof Form;
				return false;
			}
		}

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

	protected static boolean isExpansionStyleBitCorrectlySet(Section section,
			int styleBitMask, boolean newStyleBitValue) {
		int styleValue = section.getExpansionStyle();
		if (newStyleBitValue && (styleValue & styleBitMask) == 0) {
			styleValue |= styleBitMask;
		} else if (!newStyleBitValue && (styleValue & styleBitMask) != 0) {
			styleValue ^= styleBitMask;
		}
		return styleValue == section.getStyle();
	}
}
