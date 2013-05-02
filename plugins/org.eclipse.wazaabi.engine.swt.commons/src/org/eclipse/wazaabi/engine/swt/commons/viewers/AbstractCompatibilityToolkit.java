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

package org.eclipse.wazaabi.engine.swt.commons.viewers;

import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.swt.commons.views.AbstractControlDecoration;

public abstract class AbstractCompatibilityToolkit {

	/**
	 * Returns a AbstractControlDecoration which is compatible with both RAP &
	 * SWT
	 * 
	 * @param control
	 * @param position
	 * @return
	 */
	public abstract AbstractControlDecoration createControlDecoration(
			Control control, int position);

	public int getSWT_RIGHT_TO_LEFT_Value() {
		return org.eclipse.swt.SWT.NONE;
	}
}
