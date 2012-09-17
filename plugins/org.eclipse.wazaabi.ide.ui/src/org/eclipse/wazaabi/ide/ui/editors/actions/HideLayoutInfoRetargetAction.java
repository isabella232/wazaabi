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

package org.eclipse.wazaabi.ide.ui.editors.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.actions.LabelRetargetAction;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public class HideLayoutInfoRetargetAction extends LabelRetargetAction {

	/**
	 * Constructs a new UndoRetargetAction with the default ID, label and image.
	 */
	public HideLayoutInfoRetargetAction() {
		super(HideLayoutInfoAction.HIDE_LAYOUT_ACTION_ID,
				"", Action.AS_CHECK_BOX); //$NON-NLS-1$
		setImageDescriptor(Activator.getDefault().getImageRegistry()
				.getDescriptor("filter"));
		setDisabledImageDescriptor(Activator.getDefault().getImageRegistry()
				.getDescriptor("filter_disabled"));
	}

}
