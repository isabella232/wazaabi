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

package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.InPlaceCellEditor;
import org.eclipse.wazaabi.ide.propertysheets.viewers.EventsTableViewer;

public class EventHandlerCellEditor extends InPlaceCellEditor {

	FormToolkit formToolkit = null;
	private EventsTableViewer eventsTableViewer;

	public EventHandlerCellEditor(Composite parent) {
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

		eventsTableViewer = new EventsTableViewer(form.getBody(), SWT.BORDER,
				this);
		eventsTableViewer.getControl().setLayoutData(new RowData(200, 200));
		form.getBody().setLayout(new RowLayout());
		return form;
	}

	protected FormToolkit getFormToolkit() {
		return formToolkit;
	}

	@Override
	protected void setInput(Object input) {
		super.setInput(input);
		eventsTableViewer.setInput(input);
	}

	@Override
	protected void doSetFocus() {
		super.doSetFocus();
		if (eventsTableViewer != null && eventsTableViewer.getControl() != null
				&& !eventsTableViewer.getControl().isDisposed())
			eventsTableViewer.getControl().setFocus();
	}

	@Override
	public void refresh() {
		eventsTableViewer.refresh();
		super.refresh();
	}

	protected String getHeaderTitle() {
		return "Event Handlers";
	}
}
