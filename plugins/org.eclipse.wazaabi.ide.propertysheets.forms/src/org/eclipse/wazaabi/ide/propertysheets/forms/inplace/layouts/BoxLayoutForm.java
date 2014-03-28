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

package org.eclipse.wazaabi.ide.propertysheets.forms.inplace.layouts;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.RadioButtonsToEnumerationBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.TextToIntBinding;
import org.eclipse.wazaabi.ide.propertysheets.forms.inplace.AbstractDetailsSection;
import org.eclipse.wazaabi.mm.core.Orientation;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

public class BoxLayoutForm extends AbstractDetailsSection {

	private static TextToIntBinding TEXT_TO_INT_BINDING = new TextToIntBinding();
	private static RadioButtonsToEnumerationBinding RADIO_BUTTONS_TO_ENUMERATION_BINDING = new RadioButtonsToEnumerationBinding();

	@Override
	protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit().createComposite(parent,
				SWT.BORDER);
		GridLayout containerLayout = new GridLayout(1, false);
		container.setLayout(containerLayout);

		Section orientationSection = getFormToolkit().createSection(container,
				Section.TREE_NODE | Section.EXPANDED);
		GridData orientationSectionData = new GridData();
		orientationSection.setLayoutData(orientationSectionData);
		orientationSectionData.horizontalAlignment = GridData.FILL;
		orientationSectionData.grabExcessHorizontalSpace = true;
		orientationSection.setText("Orientation");
		Composite orientation = createRadioGroupField(orientationSection,
				"Orientation",
				CoreStylesPackage.Literals.BOX_LAYOUT_RULE__ORIENTATION,
				RADIO_BUTTONS_TO_ENUMERATION_BINDING, targetChangeListener);
		orientationSection.setClient(orientation);

		Section marginsSection = getFormToolkit().createSection(container,
				Section.TREE_NODE | Section.EXPANDED);
		marginsSection.setText("Margins");
		GridData marginsSectionData = new GridData();
		marginsSection.setLayoutData(marginsSectionData);
		marginsSectionData.horizontalAlignment = GridData.FILL;
		marginsSectionData.grabExcessHorizontalSpace = true;

		Composite marginsContainer = getFormToolkit().createComposite(
				marginsSection, SWT.NONE);
		marginsContainer.setLayout(new GridLayout(3, true));
		marginsSection.setClient(marginsContainer);

		Composite marginsContainerLeft = getFormToolkit().createComposite(
				marginsContainer, SWT.NONE);
		GridLayout marginsContainerLeftLayout = new GridLayout(2, false);
		marginsContainerLeft.setLayout(marginsContainerLeftLayout);
		GridData marginsContainerLeftData = new GridData();
		marginsContainerLeft.setLayoutData(marginsContainerLeftData);
		marginsContainerLeftData.horizontalAlignment = SWT.FILL;
		marginsContainerLeftData.grabExcessHorizontalSpace = true;

		Composite marginsContainerCenter = getFormToolkit().createComposite(
				marginsContainer, SWT.NONE);
		GridLayout marginsContainerCenterLayout = new GridLayout(2, false);
		marginsContainerCenter.setLayout(marginsContainerCenterLayout);
		GridData marginsContainerCenterData = new GridData();
		marginsContainerCenter.setLayoutData(marginsContainerCenterData);
		marginsContainerCenterData.horizontalAlignment = SWT.FILL;
		marginsContainerCenterData.grabExcessHorizontalSpace = true;

		Composite marginsContainerRight = getFormToolkit().createComposite(
				marginsContainer, SWT.NONE);
		GridLayout marginsContainerRightLayout = new GridLayout(2, false);
		marginsContainerRight.setLayout(marginsContainerRightLayout);
		GridData marginsContainerRightData = new GridData();
		marginsContainerRight.setLayoutData(marginsContainerRightData);
		marginsContainerRightData.horizontalAlignment = SWT.FILL;
		marginsContainerRightData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(marginsContainerLeft, "Margin:");
		Text marginTop = createTextField(marginsContainerLeft, "",
				CoreStylesPackage.Literals.BOX_LAYOUT_RULE__MARGIN,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginTopData = new GridData();
		marginTop.setLayoutData(marginTopData);
		marginTopData.horizontalAlignment = SWT.FILL;
		marginTopData.grabExcessHorizontalSpace = true;


		getFormToolkit().createLabel(marginsContainerRight, "Spacing:");
		Text marginLeft = createTextField(marginsContainerRight, "",
				CoreStylesPackage.Literals.BOX_LAYOUT_RULE__SPACING,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginLeftData = new GridData();
		marginLeft.setLayoutData(marginLeftData);
		marginLeftData.horizontalAlignment = SWT.FILL;
		marginLeftData.grabExcessHorizontalSpace = true;

		return container;
	}

	@Override
	public Object getUniqueID() {
		return CoreStylesPackage.Literals.BOX_LAYOUT_RULE;
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
		Button button = getFormToolkit().createButton(parent, text,
				SWT.BORDER | SWT.CHECK);
		bind(button, binding, feature, targetChangeListener);
		return button;
	}

	protected Composite createRadioGroupField(Composite parent, String text,
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
		return container;
	}

	@Override
	public String getTitle() {
		return "Box Layout";
	}

}