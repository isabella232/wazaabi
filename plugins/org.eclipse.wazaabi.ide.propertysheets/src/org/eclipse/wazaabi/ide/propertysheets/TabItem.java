/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets;

import org.eclipse.swt.graphics.Image;

public class TabItem implements ITabItem {

	private final String label;

	public TabItem(String label) {
		this.label = label;
	}

	public Image getImage() {
		return null;
	}

	public String getText() {
		return label != null ? label : ""; //$NON-NLS-1$
	}

	public boolean isSelected() {
		return false;
	}

	public boolean isIndented() {
		return false;
	}

}
