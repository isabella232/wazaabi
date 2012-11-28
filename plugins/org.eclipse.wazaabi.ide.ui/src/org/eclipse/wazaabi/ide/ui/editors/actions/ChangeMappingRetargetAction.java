/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
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

public class ChangeMappingRetargetAction extends LabelRetargetAction {

	public ChangeMappingRetargetAction() {
		super(ChangeMappingAction.ID, "", Action.AS_PUSH_BUTTON); //$NON-NLS-1$
		// setImageDescriptor(Activator.getDefault().getImageRegistry()
		// .getDescriptor("filter"));
		// setDisabledImageDescriptor(Activator.getDefault().getImageRegistry()
		// .getDescriptor("filter_disabled"));
	}
}
