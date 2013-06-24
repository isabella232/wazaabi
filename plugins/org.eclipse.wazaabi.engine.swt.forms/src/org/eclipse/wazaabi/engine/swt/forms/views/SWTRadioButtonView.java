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
import org.eclipse.ui.forms.widgets.FormToolkit;

public class SWTRadioButtonView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTRadioButtonView {

	private final FormToolkit formToolkit;

	public SWTRadioButtonView(FormToolkit formToolkit) {
		this.formToolkit = formToolkit;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTRadioButtonView() {
		this.formToolkit = null;
	}

	@Override
	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		Widget w = formToolkit.createButton((Composite) parent, null,
				computeSWTCreationStyle(getHost()));
		if (w instanceof org.eclipse.swt.widgets.Button)
			((org.eclipse.swt.widgets.Button) w)
					.addSelectionListener(getSelectionListener());
		return w;
	}

}
