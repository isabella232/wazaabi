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

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.wazaabi.engine.swt.forms.editparts.LabelEditPart;
import org.eclipse.wazaabi.mm.core.styles.BlankRule;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class SWTLabelView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTLabelView {

	private final SWTContainerView containingForm;

	public SWTLabelView(SWTContainerView containingForm) {
		this.containingForm = containingForm;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTLabelView() {
		this.containingForm = null;
	}

	@Override
	protected Control createLabel(Composite parent, int style) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createLabel(parent, style);
		BooleanRule renderXMLStyle = (BooleanRule) ((StyledElement) getHost()
				.getModel()).getFirstStyleRule(LabelEditPart.RENDER_XML,
				CoreStylesPackage.Literals.BOOLEAN_RULE);
		if (renderXMLStyle != null && renderXMLStyle.isValue())
			return containingForm.getFormToolkit()
					.createFormText(parent, false);
		return containingForm.getFormToolkit().createLabel((Composite) parent,
				null, computeSWTCreationStyle(getHost()));
	}

	@Override
	protected Control createLink(Composite parent, int style) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createLink(parent, style);
		return containingForm.getFormToolkit().createHyperlink(
				((org.eclipse.swt.widgets.Composite) parent), null,
				computeSWTCreationStyle(getHost()));
	}

	@Override
	protected Control wrapForSpecificParent(Composite parent, Control widget) {
		if (SWTFormsUtils.isDirectChildOfForm(getHost()))
			return widget;
		return super.wrapForSpecificParent(parent, widget);
	}

	@Override
	public void setText(StringRule rule) {
		if (getSWTWidget() instanceof FormText) {
			if (rule == null)
				((FormText) getSWTWidget()).setText("", false, false);//$NON-NLS-1$
			else
				((FormText) getSWTWidget())
						.setText(
								rule.getValue() != null ? rule.getValue() : "", true, true); //$NON-NLS-1$
			revalidate();
		} else
			super.setText(rule);
	}

	@Override
	protected void setImage(ImageRule rule) {
		if (getSWTWidget() instanceof FormText)
			return;
		super.setImage(rule);
	}

	@Override
	public void updateStyleRule(StyleRule rule) {
		// TODO Auto-generated method stub
		super.updateStyleRule(rule);
	}

	@Override
	protected boolean needReCreateWidgetView(StyleRule rule, Widget widget) {
		if (rule == null)
			return false;
		if (LabelEditPart.RENDER_XML.equals(rule.getPropertyName())) {
			if (rule instanceof BooleanRule)
				return !(((BooleanRule) rule).isValue() == (widget instanceof FormText));
			else if (rule instanceof BlankRule)
				return widget instanceof FormText;
			else
				return false;
		}
		return super.needReCreateWidgetView(rule, widget);
	}
}
