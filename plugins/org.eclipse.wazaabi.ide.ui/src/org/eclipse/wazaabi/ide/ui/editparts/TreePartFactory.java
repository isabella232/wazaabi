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

import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartFactory;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractButton;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.Label;

public class TreePartFactory implements EditPartFactory {

	public EditPart createEditPart(EditPart context, Object model) {
		EditPart newEditPart = null;
		if (model instanceof Container)
			newEditPart = new ContainerTreeEditPart();
		else if (model instanceof Label)
			newEditPart = new LabelTreeEditPart();
		else if (model instanceof AbstractButton)
			newEditPart = new AbstractButtonTreeEditPart();
		else if (model instanceof AbstractComponent)
			newEditPart = new AbstractComponentTreeEditPart();
		else if (model instanceof LayoutRule)
			newEditPart = new LayoutRuleTreeEditPart();
		else if (model instanceof LayoutDataRule)
			newEditPart = new LayoutDataRuleTreeEditPart();
		if (newEditPart != null)
			newEditPart.setModel(model);
		return newEditPart;
	}

}
