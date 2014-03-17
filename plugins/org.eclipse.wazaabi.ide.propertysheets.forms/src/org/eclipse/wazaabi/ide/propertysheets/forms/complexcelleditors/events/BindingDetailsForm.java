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

package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.events;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Hyperlink;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.AbstractDetailsSection;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

public class BindingDetailsForm extends AbstractDetailsSection {

	private static TextToFirstStringParameterBinding TEXT_TO_SOURCE_STRING_PARAMETER = new TextToFirstStringParameterBinding(
			"source"); //$NON-NLS-1$
	private static TextToFirstStringParameterBinding TEXT_TO_TARGET_STRING_PARAMETER = new TextToFirstStringParameterBinding(
			"target"); //$NON-NLS-1$

	@Override
	protected Control createSection(final Section parent,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit()
				.createComposite(parent, SWT.NONE);

		container.setLayout(new FormLayout());
		Hyperlink link = getFormToolkit().createHyperlink(container,
				"Event handler", SWT.NONE);
		FormData linkFormData = new FormData();
		linkFormData.left = new FormAttachment(0, 5);
		link.setLayoutData(linkFormData);

		final Text source = createLeftAlignedTextField(container, "",
				EDPHandlersPackage.Literals.PARAMETERIZED__PARAMETERS,
				TEXT_TO_SOURCE_STRING_PARAMETER, targetChangeListener);

		FormData uriFormData = new FormData();
		uriFormData.top = new FormAttachment(0, 0);
		uriFormData.left = new FormAttachment(link, 5);
		source.setLayoutData(uriFormData);

		linkFormData.top = new FormAttachment(source, 0, SWT.CENTER);

		Button button = getFormToolkit().createButton(container,
				"Bbbbbbrowse...", SWT.PUSH);
		FormData buttonFormData = new FormData();
		buttonFormData.top = new FormAttachment(source, 0, SWT.CENTER);
		buttonFormData.right = new FormAttachment(100, -5);

		uriFormData.right = new FormAttachment(button, -5);

		button.setLayoutData(buttonFormData);

		button.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				// SearchDeferredURIDialog dialog = new SearchDeferredURIDialog(
				// parent.getShell(), getMethodLocator(), "execute", 3,
				// uri.getText());
				// dialog.open();
				// if (dialog.getReturnCode() == Window.OK
				// && dialog.getSelected() != null)
				// uri.setText(dialog.getSelected());
			}

		});
		return container;
	}

	@Override
	public Object getUniqueID() {
		return EDPHandlersPackage.Literals.EVENT_HANDLER;
	}

	protected Text createLeftAlignedTextField(Composite parent, String text,
			EStructuralFeature feature, AbstractBinding binding,
			TargetChangeListener targetChangeListener) {
		Text field = getFormToolkit().createText(parent, "",
				SWT.LEFT | SWT.BORDER);
		bind(field, binding, feature, targetChangeListener);
		return field;
	}

	@Override
	public String getTitle() {
		return "";
	}

}
