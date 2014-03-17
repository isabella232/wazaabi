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

import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.events.BindingDetailsForm;

public class BindingCellEditor extends AbstractEventHandlerCellEditor {

	// FormToolkit formToolkit = null;
	// private EventsTableViewer eventsTableViewer;
	private BindingDetailsForm eventHandlerDetailsDescriptor;

	// private Control handlerDetailsPart;
	//
	// public EventHandlerCellEditor(Composite parent) {
	// super(parent);
	// }
	//
	// @Override
	// protected Control createControl(Composite parent) {
	// formToolkit = new FormToolkit(parent.getDisplay());
	// Form form = formToolkit.createForm(parent);
	//
	// form.addDisposeListener(new DisposeListener() {
	//
	// public void widgetDisposed(DisposeEvent e) {
	// if (getFormToolkit() != null)
	// getFormToolkit().dispose();
	// }
	// });
	//
	// form.setText(getHeaderTitle());
	// formToolkit.decorateFormHeading(form);
	//
	// form.getToolBarManager().add(createCloseAction());
	// form.getToolBarManager().update(true);
	// form.getBody().setLayout(new FormLayout());
	//
	// Section eventsSection = getFormToolkit().createSection(form.getBody(),
	// Section.TITLE_BAR | Section.EXPANDED);
	// eventsSection.setText("Events:");
	//
	// eventsTableViewer = new EventsTableViewer(eventsSection, SWT.BORDER,
	// this);
	// FormData eventsSectionFormData = new FormData();
	// eventsSectionFormData.top = new FormAttachment(0, 5);
	// eventsSectionFormData.left = new FormAttachment(0, 0);
	// eventsSectionFormData.bottom = new FormAttachment(100, -5);
	//
	// eventsSection.setLayoutData(eventsSectionFormData);
	// eventsSection.setClient(eventsTableViewer.getControl());
	//
	// Section mainSection = getFormToolkit().createSection(form.getBody(),
	// Section.TITLE_BAR | Section.EXPANDED);
	// FormData sectionFormData = new FormData();
	// sectionFormData.top = new FormAttachment(0, 5);
	// sectionFormData.bottom = new FormAttachment(100, -5);
	// sectionFormData.right = new FormAttachment(100, 0);
	// sectionFormData.left = new FormAttachment(eventsSection, 0);
	// eventsSectionFormData.right = new FormAttachment(mainSection, 0);
	//
	// mainSection.setLayoutData(sectionFormData);
	//
	// handlerDetailsPart = getEventHandlerDetailsDescriptor().createContents(
	// mainSection, this);
	// mainSection.setClient(handlerDetailsPart);
	// return form;
	// }
	//
	// protected FormToolkit getFormToolkit() {
	// return formToolkit;
	// }
	//
	// @Override
	// protected void setInput(Object input) {
	// super.setInput(input);
	// eventsTableViewer.setInput(input);
	// getEventHandlerDetailsDescriptor().setInput(getHandlerDetailsPart(),
	// (EObject) input);
	// }
	//
	// @Override
	// protected void doSetFocus() {
	// super.doSetFocus();
	// if (eventsTableViewer != null && eventsTableViewer.getControl() != null
	// && !eventsTableViewer.getControl().isDisposed())
	// eventsTableViewer.getControl().setFocus();
	// }
	//
	// @Override
	// public void refresh() {
	// eventsTableViewer.refresh();
	// getEventHandlerDetailsDescriptor().refresh(getHandlerDetailsPart());
	// super.refresh();
	// }

	public BindingCellEditor(Composite parent) {
		super(parent);
	}

	@Override
	protected String getHeaderTitle() {
		return "Binding";
	}

	@Override
	protected AbstractDetailsSection getEventHandlerDetailsDescriptor() {
		if (eventHandlerDetailsDescriptor == null)
			eventHandlerDetailsDescriptor = new BindingDetailsForm();
		return eventHandlerDetailsDescriptor;
	}

	// protected Control getHandlerDetailsPart() {
	// return handlerDetailsPart;
	// }

	// protected MethodLocator getMethodLocator() {
	// return null;
	// }

}
