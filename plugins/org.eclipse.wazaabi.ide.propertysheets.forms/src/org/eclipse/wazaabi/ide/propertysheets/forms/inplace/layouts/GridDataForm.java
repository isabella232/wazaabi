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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.CheckboxToBooleanBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.RadioButtonsToEnumerationBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.TextToIntBinding;
import org.eclipse.wazaabi.ide.propertysheets.forms.inplace.AbstractDetailsSection;
import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class GridDataForm extends AbstractDetailsSection {

	private static TextToIntBinding TEXT_TO_INT_BINDING = new TextToIntBinding();
	private static CheckboxToBooleanBinding CHECKBOX_TO_BOOLEAN_BINDING = new CheckboxToBooleanBinding();
	private static RadioButtonsToEnumerationBinding RADIO_BUTTONS_TO_ENUMERATION_BINDING = new RadioButtonsToEnumerationBinding();

	@Override
	protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit().createComposite(parent,
				SWT.BORDER);
		container.setLayout(new GridLayout(2, true));

		// horizontal section
		Section horizontalSection = getFormToolkit().createSection(container,
				Section.TREE_NODE | Section.EXPANDED);
		horizontalSection.setText("Horizontal");
		GridData horizontalSectionData = new GridData();
		horizontalSection.setLayoutData(horizontalSectionData);
		horizontalSectionData.horizontalAlignment = GridData.FILL;
		horizontalSectionData.grabExcessHorizontalSpace = true;

		Composite horizontalContainer = getFormToolkit().createComposite(
				horizontalSection, SWT.BORDER);
		horizontalContainer.setLayout(new GridLayout(2, false));
		horizontalSection.setClient(horizontalContainer);

		Composite horizontalAlignment = createRadioGroupField(
				horizontalContainer,
				"Alignment",
				SWTStylesPackage.Literals.GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT,
				RADIO_BUTTONS_TO_ENUMERATION_BINDING, targetChangeListener);
		GridData horizontalAlignmentData = new GridData();
		horizontalAlignment.setLayoutData(horizontalAlignmentData);
		horizontalAlignmentData.horizontalAlignment = SWT.FILL;
		horizontalAlignmentData.grabExcessHorizontalSpace = true;
		horizontalAlignmentData.horizontalSpan = 2;

		Composite internalHorizontalContainerLeft = getFormToolkit()
				.createComposite(horizontalContainer, SWT.BORDER);
		GridLayout internalHorizontalContainerLeftLayout = new GridLayout(2,
				false);
		internalHorizontalContainerLeft
				.setLayout(internalHorizontalContainerLeftLayout);
		GridData internalHorizontalContainerLeftData = new GridData();
		internalHorizontalContainerLeft
				.setLayoutData(internalHorizontalContainerLeftData);
		internalHorizontalContainerLeftData.horizontalAlignment = SWT.FILL;
		internalHorizontalContainerLeftData.grabExcessHorizontalSpace = true;

		Composite internalHorizontalContainerRight = getFormToolkit()
				.createComposite(horizontalContainer, SWT.BORDER);
		GridLayout otherContainerRightLayout = new GridLayout(2, false);
		internalHorizontalContainerRight.setLayout(otherContainerRightLayout);
		GridData internalHorizontalContainerRightData = new GridData();
		internalHorizontalContainerRight
				.setLayoutData(internalHorizontalContainerRightData);
		internalHorizontalContainerRightData.horizontalAlignment = SWT.FILL;
		internalHorizontalContainerRightData.grabExcessHorizontalSpace = true;

		Button grabExcessHorizontalSpace = createCheckboxField(
				internalHorizontalContainerRight,
				"grab excess space",
				SWTStylesPackage.Literals.GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		GridData grabExcessHorizontalSpaceData = new GridData();
		grabExcessHorizontalSpace.setLayoutData(grabExcessHorizontalSpaceData);
		grabExcessHorizontalSpaceData.horizontalAlignment = SWT.FILL;
		grabExcessHorizontalSpaceData.grabExcessHorizontalSpace = true;
		grabExcessHorizontalSpaceData.horizontalSpan = 2;

		getFormToolkit().createLabel(internalHorizontalContainerRight, "Span:");
		Text horizontalSpan = createTextField(internalHorizontalContainerRight,
				"", SWTStylesPackage.Literals.GRID_DATA_RULE__HORIZONTAL_SPAN,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData horizontalSpanData = new GridData();
		horizontalSpan.setLayoutData(horizontalSpanData);
		horizontalSpanData.horizontalAlignment = SWT.FILL;
		horizontalSpanData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(internalHorizontalContainerLeft,
				"Width hint:");
		Text widthHint = createTextField(internalHorizontalContainerLeft, "",
				SWTStylesPackage.Literals.GRID_DATA_RULE__WIDTH_HINT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData widthHintData = new GridData();
		widthHint.setLayoutData(widthHintData);
		widthHintData.horizontalAlignment = SWT.FILL;
		widthHintData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(internalHorizontalContainerLeft,
				"Width hint:");
		Text minimumHint = createTextField(internalHorizontalContainerLeft, "",
				SWTStylesPackage.Literals.GRID_DATA_RULE__MINIMUM_WIDTH,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData minimumHintData = new GridData();
		minimumHint.setLayoutData(minimumHintData);
		minimumHintData.horizontalAlignment = SWT.FILL;
		minimumHintData.grabExcessHorizontalSpace = true;

		// vertical section
		Section verticalSection = getFormToolkit().createSection(container,
				Section.TREE_NODE | Section.EXPANDED);
		verticalSection.setText("Vertical");
		GridData verticalSectionData = new GridData();
		verticalSection.setLayoutData(verticalSectionData);
		verticalSectionData.horizontalAlignment = GridData.FILL;
		verticalSectionData.grabExcessHorizontalSpace = true;

		Composite verticalContainer = getFormToolkit().createComposite(
				verticalSection, SWT.NONE);
		verticalContainer.setLayout(new GridLayout(2, true));
		verticalSection.setClient(verticalContainer);

		Composite verticallAlignment = createRadioGroupField(verticalContainer,
				"Alignment",
				SWTStylesPackage.Literals.GRID_DATA_RULE__VERTICAL_ALIGNEMENT,
				RADIO_BUTTONS_TO_ENUMERATION_BINDING, targetChangeListener);
		GridData verticallAlignmentData = new GridData();
		verticallAlignment.setLayoutData(verticallAlignmentData);
		verticallAlignmentData.horizontalAlignment = SWT.FILL;
		verticallAlignmentData.grabExcessHorizontalSpace = true;
		verticallAlignmentData.horizontalSpan = 2;

		Composite internalVerticalContainerLeft = getFormToolkit()
				.createComposite(verticalContainer, SWT.BORDER);
		GridLayout internalVerticalContainerLeftLayout = new GridLayout(2,
				false);
		internalVerticalContainerLeft
				.setLayout(internalVerticalContainerLeftLayout);
		GridData internalVerticalContainerLeftData = new GridData();
		internalVerticalContainerLeft
				.setLayoutData(internalVerticalContainerLeftData);
		internalVerticalContainerLeftData.horizontalAlignment = SWT.FILL;
		internalVerticalContainerLeftData.grabExcessHorizontalSpace = true;

		Composite internalVerticalContainerRight = getFormToolkit()
				.createComposite(verticalContainer, SWT.BORDER);
		GridLayout internalVerticalContainerRightLayout = new GridLayout(2,
				false);
		internalVerticalContainerRight
				.setLayout(internalVerticalContainerRightLayout);
		GridData internalVerticalContainerRightLayoutData = new GridData();
		internalVerticalContainerRight
				.setLayoutData(internalVerticalContainerRightLayoutData);
		internalVerticalContainerRightLayoutData.horizontalAlignment = SWT.FILL;
		internalVerticalContainerRightLayoutData.grabExcessHorizontalSpace = true;

		Button grabExcessVerticalSpace = createCheckboxField(
				internalVerticalContainerRight,
				"grab excess space",
				SWTStylesPackage.Literals.GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		GridData grabExcessVerticalSpaceData = new GridData();
		grabExcessVerticalSpace.setLayoutData(grabExcessVerticalSpaceData);
		grabExcessVerticalSpaceData.horizontalAlignment = SWT.FILL;
		grabExcessVerticalSpaceData.grabExcessHorizontalSpace = true;
		grabExcessVerticalSpaceData.horizontalSpan = 2;

		getFormToolkit().createLabel(internalVerticalContainerRight, "Span:");
		Text verticalSpan = createTextField(internalVerticalContainerRight, "",
				SWTStylesPackage.Literals.GRID_DATA_RULE__VERTICAL_SPAN,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData verticalSpanData = new GridData();
		verticalSpan.setLayoutData(verticalSpanData);
		verticalSpanData.horizontalAlignment = SWT.FILL;
		verticalSpanData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(internalVerticalContainerLeft,
				"Height hint:");
		Text heightHint = createTextField(internalVerticalContainerLeft, "",
				SWTStylesPackage.Literals.GRID_DATA_RULE__HEIGHT_HINT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData heightHintData = new GridData();
		heightHint.setLayoutData(heightHintData);
		heightHintData.horizontalAlignment = SWT.FILL;
		heightHintData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(internalVerticalContainerLeft,
				"Minimum hint:");
		Text minimumHeightHint = createTextField(internalVerticalContainerLeft,
				"", SWTStylesPackage.Literals.GRID_DATA_RULE__MINIMUM_HEIGHT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData minimumHeightHintData = new GridData();
		minimumHeightHint.setLayoutData(minimumHeightHintData);
		minimumHeightHintData.horizontalAlignment = SWT.FILL;
		minimumHeightHintData.grabExcessHorizontalSpace = true;

		Button exclude = createCheckboxField(container, "Exclude",
				SWTStylesPackage.Literals.GRID_DATA_RULE__EXCLUDE,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		GridData excludeData = new GridData();
		exclude.setLayoutData(excludeData);
		excludeData.horizontalAlignment = SWT.CENTER;
		excludeData.grabExcessHorizontalSpace = true;
		excludeData.horizontalSpan = 2;

		return container;
	}

	@Override
	public Object getUniqueID() {
		return SWTStylesPackage.Literals.GRID_DATA_RULE;
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
		Composite container = new Group(parent, SWT.NONE);
		((Group) container).setText("Alignment");
		container.setLayout(new RowLayout());
		Button begginning = getFormToolkit().createButton(container,
				"Begginning", SWT.RADIO);
		begginning.setData(
				RadioButtonsToEnumerationBinding.ENUMERATION_VALUE_KEY,
				GridDataAlignment.BEGINNING);
		Button center = getFormToolkit().createButton(container, "Center",
				SWT.RADIO);
		center.setData(RadioButtonsToEnumerationBinding.ENUMERATION_VALUE_KEY,
				GridDataAlignment.CENTER);
		Button end = getFormToolkit().createButton(container, "End", SWT.RADIO);
		end.setData(RadioButtonsToEnumerationBinding.ENUMERATION_VALUE_KEY,
				GridDataAlignment.END);
		Button fill = getFormToolkit().createButton(container, "Fill",
				SWT.RADIO);
		fill.setData(RadioButtonsToEnumerationBinding.ENUMERATION_VALUE_KEY,
				GridDataAlignment.FILL);
		bind(container, binding, feature, targetChangeListener);
		return container;
	}

	@Override
	public String getTitle() {
		return "Grid Data";
	}
}
