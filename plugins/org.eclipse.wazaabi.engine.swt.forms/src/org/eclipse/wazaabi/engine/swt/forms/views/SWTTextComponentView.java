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
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

public class SWTTextComponentView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTTextComponentView {

	private final SWTContainerView containingForm;

	public SWTTextComponentView(SWTContainerView containingForm) {
		this.containingForm = containingForm;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTTextComponentView() {
		this.containingForm = null;
	}

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		if (containingForm == null || containingForm.getFormToolkit() == null)
			return super.createSWTWidget(parent, swtStyle, index);
		final Text text = containingForm.getFormToolkit().createText(
				(org.eclipse.swt.widgets.Composite) parent, null,
				computeSWTCreationStyle(getHost()));
		if (getModifyListener() != null)
			text.addModifyListener(getModifyListener());
		if (SWTFormsUtils.isDirectChildOfForm(getHost()))
			return text;
		return wrapForSpecificParent((Composite) parent, text);
	}

}
