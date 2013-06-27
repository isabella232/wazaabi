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

import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;

public class SWTPushButtonView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTPushButtonView {

	private final SWTContainerView containingForm;

	public SWTPushButtonView(SWTContainerView containingForm) {
		this.containingForm = containingForm;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTPushButtonView() {
		this.containingForm = null;
	}

	@Override
	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createSWTWidget(parent, swtStyle, index);
		Button button = containingForm.getFormToolkit().createButton(
				(Composite) parent, null, computeSWTCreationStyle(getHost()));
		if (SWTFormsUtils.isDirectChildOfForm(getHost()))
			return button;
		return wrapForSpecificParent((Composite) parent, button);
	}
}
