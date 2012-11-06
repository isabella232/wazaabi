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

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.gef.EditPart;
import org.eclipse.gef.ui.parts.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.wazaabi.ide.ui.editparts.LayoutDataRuleTreeEditPart;
import org.eclipse.wazaabi.ide.ui.editparts.LayoutRuleTreeEditPart;

public class ExtendedTreeViewer extends TreeViewer {

	private boolean displayLayoutInfo = true;
	
	public void setDisplayLayoutInfo(boolean value) {
		boolean previousValue = isDisplayingLayoutInfo();
		this.displayLayoutInfo = value;
		if (previousValue != value)
			forceDeepLayoutInfosRefresh();
	}

	public ExtendedTreeViewer() {
		super();
		addDropTargetListener(new LocalTransferDropTargetListener(this));
	}

	public boolean isDisplayingLayoutInfo() {
		return this.displayLayoutInfo;
	}

	@Override
	public Control createControl(Composite parent) {
		Tree tree = new Tree(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		setControl(tree);
		return tree;
	}

	@SuppressWarnings("unchecked")
	protected void forceDeepLayoutInfosRefresh() {
		List<EditPart> editParts = new ArrayList<EditPart>();
		editParts.addAll(getEditPartRegistry().values());
		for (EditPart ep : editParts)
			if (ep instanceof LayoutRuleTreeEditPart
					|| ep instanceof LayoutDataRuleTreeEditPart)
				continue;
			else
				ep.refresh();

	}
}
