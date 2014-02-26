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

package org.eclipse.wazaabi.ide.propertysheets.forms.table.editinghelpers.complexcelleditors.details;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings.CheckboxToBooleanBinding;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings.RadioButtonsToEnumerationBinding;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.complexcelleditors.bindings.TextToIntBinding;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class RowLayoutForm extends AbstractDetailsSection {

	private static TextToIntBinding TEXT_TO_INT_BINDING = new TextToIntBinding();
	private static CheckboxToBooleanBinding CHECKBOX_TO_BOOLEAN_BINDING = new CheckboxToBooleanBinding();
	private static RadioButtonsToEnumerationBinding RADIO_BUTTONS_TO_ENUMERATION_BINDING = new RadioButtonsToEnumerationBinding();

	@Override
	protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit().createComposite(parent);
		container.setLayout(new GridLayout(2, true));
		createTextField(container, "margin bottom:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__MARGIN_BOTTOM,
				TEXT_TO_INT_BINDING, targetChangeListener);
		createTextField(container, "margin height:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__MARGIN_HEIGHT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		createTextField(container, "margin left:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__MARGIN_LEFT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		createTextField(container, "margin right:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__MARGIN_RIGHT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		createTextField(container, "margin top:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__MARGIN_TOP,
				TEXT_TO_INT_BINDING, targetChangeListener);
		createTextField(container, "margin width:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__MARGIN_WIDTH,
				TEXT_TO_INT_BINDING, targetChangeListener);
		createCheckboxField(container, "center:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__CENTER,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		createCheckboxField(container, "fill:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__FILL,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		createCheckboxField(container, "justify:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__JUSTIFY,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		createCheckboxField(container, "pack:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__PACK,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		createTextField(container, "spacing:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__SPACING,
				TEXT_TO_INT_BINDING, targetChangeListener);

		createRadioGroupField(container, "type:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__TYPE,
				RADIO_BUTTONS_TO_ENUMERATION_BINDING, targetChangeListener);

		createCheckboxField(container, "wrap:",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE__WRAP,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);

		return container;
	}

	@Override
	public Object getUniqueID() {
		return SWTStylesPackage.Literals.ROW_LAYOUT_RULE;
	}

	protected void createTextField(Composite parent, String text,
			EStructuralFeature feature, AbstractBinding binding,
			TargetChangeListener targetChangeListener) {
		getFormToolkit().createLabel(parent, text);
		Text field = getFormToolkit().createText(parent, "", SWT.BORDER);
		bind(field, binding, feature, targetChangeListener);
	}

	protected void createCheckboxField(Composite parent, String text,
			EStructuralFeature feature, AbstractBinding binding,
			TargetChangeListener targetChangeListener) {
		Button button = getFormToolkit().createButton(parent, text,
				SWT.BORDER | SWT.CHECK);
		bind(button, binding, feature, targetChangeListener);
	}

	protected void createRadioGroupField(Composite parent, String text,
			EStructuralFeature feature, AbstractBinding binding,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit().createComposite(parent);
		container.setLayout(new RowLayout());
		Button button1 = getFormToolkit().createButton(container, "horizontal",
				SWT.RADIO);
		button1.setData(RadioButtonsToEnumerationBinding.ENUMERATION_VALUE_KEY,
				Orientation.HORIZONTAL);
		Button button2 = getFormToolkit().createButton(container, "vertical",
				SWT.RADIO);
		button2.setData(RadioButtonsToEnumerationBinding.ENUMERATION_VALUE_KEY,
				Orientation.VERTICAL);
		bind(container, binding, feature, targetChangeListener);
	}
}
