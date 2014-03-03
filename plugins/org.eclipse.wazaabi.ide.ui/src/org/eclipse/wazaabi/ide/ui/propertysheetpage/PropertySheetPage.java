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

package org.eclipse.wazaabi.ide.ui.propertysheetpage;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.wazaabi.ide.propertysheets.forms.table.FormBasedStyleRuleTableViewer;
import org.eclipse.wazaabi.ide.ui.editors.WazaabiTreeEditor;
import org.eclipse.wazaabi.ide.ui.editparts.AbstractTreeEditPart;

public class PropertySheetPage implements IPropertySheetPage {
	private FormBasedStyleRuleTableViewer viewer;

	private WazaabiTreeEditor currentWazaabiTreeEditor = null;

	@Override
	public void createControl(Composite parent) {
		// viewer = new FormBasedStyleRuleTableViewer(parent);
	}

	@Override
	public void dispose() {
		if (viewer != null & viewer.getControl() != null
				&& !viewer.getControl().isDisposed())
			viewer.getControl().dispose();
	}

	@Override
	public Control getControl() {
		return viewer != null ? viewer.getControl() : null;
	}

	@Override
	public void setActionBars(IActionBars actionBars) {

	}

	@Override
	public void setFocus() {
		if (viewer != null & viewer.getControl() != null
				&& !viewer.getControl().isDisposed())
			viewer.getControl().setFocus();

	}

	@Override
	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
		if (currentWazaabiTreeEditor != part) {
			if (currentWazaabiTreeEditor != null)
				viewer.removeTargetChangeListener(currentWazaabiTreeEditor);
			if (part instanceof WazaabiTreeEditor) {
				currentWazaabiTreeEditor = (WazaabiTreeEditor) part;
				viewer.addTargetChangeListener(currentWazaabiTreeEditor);
			}
		}
		if (part instanceof WazaabiTreeEditor
				&& ((StructuredSelection) selection).getFirstElement() instanceof AbstractTreeEditPart)
			viewer.setInput(((AbstractTreeEditPart) ((StructuredSelection) selection)
					.getFirstElement()).getModel());
	}

	public void refresh() {
		if (viewer != null)
			viewer.refresh();

	}

}
