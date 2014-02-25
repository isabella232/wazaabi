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
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.wazaabi.ide.propertysheets.forms.table.FormBasedStyleRuleTableViewer;
import org.eclipse.wazaabi.ide.ui.editparts.AbstractTreeEditPart;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class PropertySheetPage implements IPropertySheetPage {
	private FormBasedStyleRuleTableViewer viewer;
	private Composite container;

	@Override
	public void createControl(Composite parent) {
		container = new Composite(parent, SWT.NONE);
		container.setLayout(new FillLayout());
		try {
			viewer = new FormBasedStyleRuleTableViewer(container);
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}

	@Override
	public void dispose() {
		container.dispose();
	}

	@Override
	public Control getControl() {
		return container;
	}

	@Override
	public void setActionBars(IActionBars actionBars) {

	}

	@Override
	public void setFocus() {
		container.setFocus();

	}

	@Override
	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
		if (((StructuredSelection) selection).getFirstElement() instanceof AbstractTreeEditPart)
			viewer.setInput(((AbstractTreeEditPart) ((StructuredSelection) selection)
					.getFirstElement()).getModel());
		System.out.println(selection);
	}

}
