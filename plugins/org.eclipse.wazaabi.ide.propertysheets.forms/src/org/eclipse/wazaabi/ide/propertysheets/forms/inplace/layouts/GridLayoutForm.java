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
import org.eclipse.swt.layout.FillLayout;
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
import org.eclipse.wazaabi.ide.propertysheets.forms.inplace.AbstractDetailsSection;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class GridLayoutForm extends AbstractDetailsSection {

	private static TextToIntBinding TEXT_TO_INT_BINDING = new TextToIntBinding();
	private static CheckboxToBooleanBinding CHECKBOX_TO_BOOLEAN_BINDING = new CheckboxToBooleanBinding();

	@Override
	protected Control createSection(Section parent,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit().createComposite(parent,
				SWT.BORDER);
		GridLayout containerLayout = new GridLayout(1, false);
		container.setLayout(containerLayout);

		Section otherSection = getFormToolkit().createSection(container,
				Section.TREE_NODE | Section.EXPANDED);
		otherSection.setText("Main");
		GridData otherSectionData = new GridData();
		otherSection.setLayoutData(otherSectionData);
		otherSectionData.horizontalAlignment = GridData.FILL;
		otherSectionData.grabExcessHorizontalSpace = true;

		Composite otherContainer = getFormToolkit().createComposite(
				otherSection, SWT.NONE);
		otherContainer.setLayout(new FillLayout(SWT.HORIZONTAL));
		otherSection.setClient(otherContainer);

		Composite otherContainerLeft = getFormToolkit().createComposite(
				otherContainer, SWT.NONE);
		GridLayout otherContainerLeftLayout = new GridLayout(2, false);
		otherContainerLeft.setLayout(otherContainerLeftLayout);

		Composite otherContainerRight = getFormToolkit().createComposite(
				otherContainer, SWT.NONE);
		GridLayout otherContainerRightLayout = new GridLayout(2, false);
		otherContainerRight.setLayout(otherContainerRightLayout);

		getFormToolkit().createLabel(otherContainerLeft, "Num columns:");

		Text numColumns = createTextField(otherContainerLeft, "",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__NUM_COLUMNS,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData numColumnsData = new GridData();
		numColumns.setLayoutData(numColumnsData);
		numColumnsData.horizontalAlignment = SWT.FILL;
		numColumnsData.grabExcessHorizontalSpace = true;

		Button makeColumnsEqualWidth = createCheckboxField(
				otherContainerRight,
				"Equal width",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__MAKE_COLUMNS_EQUAL_WIDTH,
				CHECKBOX_TO_BOOLEAN_BINDING, targetChangeListener);
		GridData makeColumnsEqualWidthData = new GridData();
		makeColumnsEqualWidth.setLayoutData(makeColumnsEqualWidthData);
		makeColumnsEqualWidthData.horizontalAlignment = SWT.FILL;
		makeColumnsEqualWidthData.grabExcessHorizontalSpace = true;
		makeColumnsEqualWidthData.horizontalSpan = 2;

		getFormToolkit().createLabel(otherContainerLeft, "Horizontal spacing:");

		Text horizontalSpacing = createTextField(otherContainerLeft, "",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__NUM_COLUMNS,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData horizontalSpacingData = new GridData();
		horizontalSpacing.setLayoutData(horizontalSpacingData);
		horizontalSpacingData.horizontalAlignment = SWT.FILL;
		horizontalSpacingData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(otherContainerRight, "Vertical spacing:");

		Text verticalSpacing = createTextField(otherContainerRight, "",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__VERTICAL_SPACING,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData verticalSpacingData = new GridData();
		verticalSpacing.setLayoutData(verticalSpacingData);
		verticalSpacingData.horizontalAlignment = SWT.FILL;
		verticalSpacingData.grabExcessHorizontalSpace = true;
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

		getFormToolkit().createLabel(marginsContainerLeft, "Top:");
		Text marginTop = createTextField(marginsContainerLeft, "margin top:",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__MARGIN_TOP,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginTopData = new GridData();
		marginTop.setLayoutData(marginTopData);
		marginTopData.horizontalAlignment = SWT.FILL;
		marginTopData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(marginsContainerLeft, "Bottom:");
		Text marginBottom = createTextField(marginsContainerLeft,
				"margin bottom:",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__MARGIN_BOTTOM,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginBottonData = new GridData();
		marginBottom.setLayoutData(marginBottonData);
		marginBottonData.horizontalAlignment = SWT.FILL;
		marginBottonData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(marginsContainerCenter, "Left:");
		Text marginLeft = createTextField(marginsContainerCenter,
				"margin left:",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__MARGIN_LEFT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginLeftData = new GridData();
		marginLeft.setLayoutData(marginLeftData);
		marginLeftData.horizontalAlignment = SWT.FILL;
		marginLeftData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(marginsContainerCenter, "Right:");
		Text marginRight = createTextField(marginsContainerCenter,
				"margin right:",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__MARGIN_RIGHT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginRightData = new GridData();
		marginRight.setLayoutData(marginRightData);
		marginRightData.horizontalAlignment = SWT.FILL;
		marginRightData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(marginsContainerRight, "Height:");
		Text marginHeight = createTextField(marginsContainerRight,
				"margin height:",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__MARGIN_HEIGHT,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginHeightData = new GridData();
		marginHeight.setLayoutData(marginHeightData);
		marginHeightData.horizontalAlignment = SWT.FILL;
		marginHeightData.grabExcessHorizontalSpace = true;

		getFormToolkit().createLabel(marginsContainerRight, "Width:");
		Text marginWidth = createTextField(marginsContainerRight,
				"margin width:",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE__MARGIN_WIDTH,
				TEXT_TO_INT_BINDING, targetChangeListener);
		GridData marginWidthData = new GridData();
		marginWidth.setLayoutData(marginWidthData);
		marginWidthData.horizontalAlignment = SWT.FILL;
		marginWidthData.grabExcessHorizontalSpace = true;

		return container;
	}

	@Override
	public Object getUniqueID() {
		return SWTStylesPackage.Literals.GRID_LAYOUT_RULE;
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
		return "Grid Layout";
	}
}