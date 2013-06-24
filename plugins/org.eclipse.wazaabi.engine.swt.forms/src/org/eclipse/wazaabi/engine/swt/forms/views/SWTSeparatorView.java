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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Widget;

public class SWTSeparatorView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTSeparatorView {


	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		Label label = new Label((org.eclipse.swt.widgets.Composite) parent,
				computeSWTCreationStyle(getHost()) | SWT.SEPARATOR);
		return wrapForSpecificParent((Composite) parent, label);
	}

}
