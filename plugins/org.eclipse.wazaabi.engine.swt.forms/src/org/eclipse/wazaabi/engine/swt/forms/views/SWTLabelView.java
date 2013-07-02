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
		return containingForm.getFormToolkit().createLabel(
				(org.eclipse.swt.widgets.Composite) parent, null,
				computeSWTCreationStyle(getHost()));
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

}
