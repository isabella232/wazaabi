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

import java.util.Collections;
import java.util.List;

import org.eclipse.gef.EditPolicy;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.wazaabi.ide.ui.editpolicies.ComponentEditPolicy;
import org.eclipse.wazaabi.ide.ui.editpolicies.TreeEditPolicy;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public class LayoutDataRuleTreeEditPart extends AbstractTreeEditPart {

	/**
	 * Creates and installs pertinent EditPolicies for this.
	 */
	protected void createEditPolicies() {
		installEditPolicy(EditPolicy.COMPONENT_ROLE, new ComponentEditPolicy());
		installEditPolicy(EditPolicy.PRIMARY_DRAG_ROLE, new TreeEditPolicy());
	}

	/**
	 * Returns <code>null</code> as a Tree EditPart holds no children under it.
	 * 
	 * @return <code>null</code>
	 */
	protected List<?> getModelChildren() {
		return Collections.EMPTY_LIST;
	}

	/**
	 * Refreshes the visual properties of the TreeItem for this part.
	 */
	protected void refreshVisuals() {
		if (getWidget() instanceof Tree)
			return;
		Image image = Activator.getDefault().getImageRegistry()
				.get(getLayoutDataRuleModel().eClass().getName());
		TreeItem item = (TreeItem) getWidget();
		if (image != null)
			image.setBackground(item.getParent().getBackground());
		setWidgetImage(image);
		setWidgetText(getLayoutDataRuleModel().eClass().getName());
	}

	public LayoutDataRule getLayoutDataRuleModel() {
		return (LayoutDataRule) getModel();
	}

}
