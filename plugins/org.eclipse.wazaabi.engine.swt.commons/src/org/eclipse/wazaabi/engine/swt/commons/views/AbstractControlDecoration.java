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

package org.eclipse.wazaabi.engine.swt.commons.views;

import org.eclipse.swt.widgets.Control;

/**
 * This class is a facade for JFace Control Decoration. RAP & SWT do not share
 * the same interface.
 * 
 * @author olivier
 * 
 */
public abstract class AbstractControlDecoration extends
		org.eclipse.jface.fieldassist.ControlDecoration {

	private static final long serialVersionUID = 1L;

	public AbstractControlDecoration(Control control, int position) {
		super(control, position);
	}

	public abstract void update();

}
