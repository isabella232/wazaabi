/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;

public class SWTSliderView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTSliderView {

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		int style = computeSWTCreationStyle(getHost());
		// if no orientation has been given, we set it by default to HORIZONTAL
		if ((style & SWT.HORIZONTAL) == 0 && ((style & SWT.VERTICAL) == 0))
			style |= SWT.HORIZONTAL;

		final org.eclipse.swt.widgets.Slider slider = new org.eclipse.swt.widgets.Slider(
				(org.eclipse.swt.widgets.Composite) parent, style);
		if (getSelectionListener() != null)
			slider.addSelectionListener(getSelectionListener());

		return wrapForSpecificParent((Composite) parent, slider);
	}
}
