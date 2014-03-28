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

package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.layouts;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.CheckboxToBooleanBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.TextToIntBinding;
import org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors.AbstractDetailsSection;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class RowDataForm extends AbstractDetailsSection {

	private static TextToIntBinding TEXT_TO_INT_BINDING = new TextToIntBinding();
	private static CheckboxToBooleanBinding CHECKBOX_TO_BOOLEAN_BINDING = new CheckboxToBooleanBinding();

	@Override
	protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit().createComposite(parent,
				SWT.BORDER);
		GridLayout containerLayout = new GridLayout(2, false);
		container.setLayout(containerLayout);

		getFormToolkit().createLabel(container, "Height:");
		Text height = createTextField(container, "",
				SWTStylesPackage.Literals.ROW_DATA_RULE__HEIGHT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData heightData = new GridData();
		height.setLayoutData(heightData);
		heightData.horizontalAlignment = SWT.FILL;
		heightData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(container, "Width:");
		Text width = createTextField(container, "",
				SWTStylesPackage.Literals.ROW_DATA_RULE__WIDTH,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData widthData = new GridData();
		width.setLayoutData(widthData);
		widthData.horizontalAlignment = SWT.FILL;
		widthData.grabExcessHorizontalSpace = true;

		Button exclude = createCheckboxField(container, "Exclude",
				SWTStylesPackage.Literals.ROW_DATA_RULE__EXCLUDE,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		GridData excludeData = new GridData();
		exclude.setLayoutData(excludeData);
		excludeData.horizontalAlignment = SWT.FILL;
		excludeData.grabExcessHorizontalSpace = true;
		excludeData.horizontalSpan = 2;
		return container;
	}

	@Override
	public Object getUniqueID() {
		return SWTStylesPackage.Literals.ROW_DATA_RULE;
	}

	protected Text createTextField(Composite parent, String text,
			EStructuralFeature feature, AbstractBinding binding,
			TargetChangeListener targetChangeListener) {
		Text field = getFormToolkit().createText(parent, "",
				SWT.RIGHT | SWT.BORDER);
		bind(field, binding, feature, targetChangeListener);
		return field;
	}

	protected Button createCheckboxField(Composite parent, String text,
			EStructuralFeature feature, AbstractBinding binding,
			TargetChangeListener targetChangeListener) {
		Button button = getFormToolkit().createButton(parent, text, SWT.CHECK);
		bind(button, binding, feature, targetChangeListener);
		return button;
	}

	@Override
	public String getTitle() {
		return "Row Data";
	}
}
