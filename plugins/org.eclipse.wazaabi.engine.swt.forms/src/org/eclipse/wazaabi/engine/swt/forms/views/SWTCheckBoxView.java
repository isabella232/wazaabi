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
import org.eclipse.swt.widgets.Widget;

public class SWTCheckBoxView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTCheckBoxView {

	private final SWTContainerView containingForm;

	public SWTCheckBoxView(SWTContainerView containingForm) {
		this.containingForm = containingForm;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTCheckBoxView() {
		this.containingForm = null;
	}

	@Override
	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createSWTWidget(parent, swtStyle, index);
		Widget w = containingForm.getFormToolkit().createButton(
				(Composite) parent, null, computeSWTCreationStyle(getHost()));
		if (w instanceof org.eclipse.swt.widgets.Button)
			((org.eclipse.swt.widgets.Button) w)
					.addSelectionListener(getSelectionListener());
		return w;
	}

}
