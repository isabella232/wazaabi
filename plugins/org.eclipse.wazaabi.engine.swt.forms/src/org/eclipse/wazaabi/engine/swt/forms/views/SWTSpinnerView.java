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

import org.eclipse.ui.forms.widgets.FormToolkit;

public class SWTSpinnerView extends
		org.eclipse.wazaabi.engine.swt.commons.views.SWTSpinnerView {

	private final FormToolkit formToolkit;

	public SWTSpinnerView(FormToolkit formToolkit) {
		this.formToolkit = formToolkit;
	}

	/**
	 * private for avoiding the use of this constructor
	 */
	@SuppressWarnings("unused")
	private SWTSpinnerView() {
		this.formToolkit = null;
	}	
	
	// protected Widget createSWTWidget(Widget parent, int swtStyle, int index)
	// {
	// int style = computeSWTCreationStyle(getHost());
	//
	// final org.eclipse.swt.widgets.Spinner spinner = new
	// org.eclipse.swt.widgets.Spinner(
	// (org.eclipse.swt.widgets.Composite) parent, style);
	// if (getSelectionListener() != null)
	// spinner.addSelectionListener(getSelectionListener());
	//
	// IntRule max = (IntRule) ((Spinner) getHost().getModel())
	// .getFirstStyleRule(SpinnerEditPart.MAXIMUM_PROPERTY_NAME, null);
	// if (max != null) {
	// spinner.setMaximum(max.getValue());
	// }
	// IntRule min = (IntRule) ((Spinner) getHost().getModel())
	// .getFirstStyleRule(SpinnerEditPart.MINIMUM_PROPERTY_NAME, null);
	// if (min != null) {
	// spinner.setMinimum(min.getValue());
	// }
	//
	// return wrapForSpecificParent((Composite) parent, spinner);
	// }

}
