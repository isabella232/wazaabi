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

public class SWTProgressBarView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTProgressBarView {

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		int style = computeSWTCreationStyle(getHost());
		// if no orientation has been given, we set it by default to HORIZONTAL
		if ((style & SWT.HORIZONTAL) == 0 && ((style & SWT.VERTICAL) == 0))
			style |= SWT.HORIZONTAL;
		org.eclipse.swt.widgets.ProgressBar progressBar = new org.eclipse.swt.widgets.ProgressBar(
				(org.eclipse.swt.widgets.Composite) parent, style);
		return wrapForSpecificParent((Composite) parent, progressBar);
	}
}
