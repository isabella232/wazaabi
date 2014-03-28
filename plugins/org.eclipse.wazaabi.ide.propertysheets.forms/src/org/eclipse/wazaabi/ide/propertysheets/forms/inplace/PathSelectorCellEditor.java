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

package org.eclipse.wazaabi.ide.propertysheets.forms.inplace;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.InPlaceCellEditor;

public class PathSelectorCellEditor extends InPlaceCellEditor {

	FormToolkit formToolkit = null;

	public PathSelectorCellEditor(Composite parent) {
		super(parent);
	}

	@Override
	protected Control createControl(Composite parent) {
		formToolkit = new FormToolkit(parent.getDisplay());
		Form form = formToolkit.createForm(parent);

		form.addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				if (getFormToolkit() != null)
					getFormToolkit().dispose();
			}
		});

		form.setText(getHeaderTitle());
		formToolkit.decorateFormHeading(form);

		form.getToolBarManager().add(createCloseAction());
		form.getToolBarManager().update(true);
		form.getBody().setLayout(new FormLayout());
		return form;
	}

	protected FormToolkit getFormToolkit() {
		return formToolkit;
	}

	protected String getHeaderTitle() {
		return "Path Selector";
	}
}
