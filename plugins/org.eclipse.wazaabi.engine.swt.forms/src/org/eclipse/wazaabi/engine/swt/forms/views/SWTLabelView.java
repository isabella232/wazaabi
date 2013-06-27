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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.LabelEditPart;
import org.eclipse.wazaabi.mm.core.styles.HyperlinkRule;
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

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createSWTWidget(parent, swtStyle, index);
		StyleRule lookandfeel = ((StyledElement) getHost().getModel())
				.getFirstStyleRule(LabelEditPart.LOOKANDFEEL_PROPERTY_NAME,
						null);
		if (lookandfeel != null) {
			if (lookandfeel instanceof HyperlinkRule)
				return containingForm.getFormToolkit().createHyperlink(
						((org.eclipse.swt.widgets.Composite) parent), null,
						computeSWTCreationStyle(getHost()));
		}
		Label label = containingForm.getFormToolkit().createLabel(
				(org.eclipse.swt.widgets.Composite) parent, null,
				computeSWTCreationStyle(getHost()));
		if (SWTFormsUtils.isDirectChildOfForm(getHost()))
			return label;
		return wrapForSpecificParent((Composite) parent, label);

	}
}
