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

package org.eclipse.wazaabi.ide.ui.editparts;

import org.eclipse.gef.editparts.RootTreeEditPart;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;

public class RootTreeEditPartWithOneChild extends RootTreeEditPart {

	@Override
	public void setWidget(Widget w) {
		if (w instanceof Tree)
			super.setWidget(new TreeItem((Tree) w, SWT.NONE));
		else if (w == null) {
			if (getWidget() != null && getWidget().isDisposed())
				getWidget().dispose();
			super.setWidget(null);
		}
	}
}
