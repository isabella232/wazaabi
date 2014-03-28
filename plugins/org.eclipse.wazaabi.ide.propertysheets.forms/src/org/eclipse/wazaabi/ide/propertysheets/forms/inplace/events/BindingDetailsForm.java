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

package org.eclipse.wazaabi.ide.propertysheets.forms.inplace.events;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.forms.inplace.AbstractDetailsSection;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

public class BindingDetailsForm extends AbstractDetailsSection {

	@Override
	protected Control createSection(final Section parent,
			TargetChangeListener targetChangeListener) {
		Composite container = getFormToolkit()
				.createComposite(parent, SWT.NONE);

		container.setLayout(new FormLayout());

		Label sourceLabel = getFormToolkit().createLabel(container, "Source:");
		FormData sourceLabelFormData = new FormData();
		sourceLabelFormData.left = new FormAttachment(0, 5);
		sourceLabel.setLayoutData(sourceLabelFormData);

		final Text source = createLeftAlignedTextField(container, "",
				EDPHandlersPackage.Literals.PARAMETERIZED__PARAMETERS,
				new TextToFirstStringParameterBinding("source"), //$NON-NLS-1$
				targetChangeListener);

		FormData sourceFormData = new FormData();
		sourceFormData.top = new FormAttachment(0, 0);
		sourceFormData.left = new FormAttachment(sourceLabel, 5);
		sourceFormData.right = new FormAttachment(100, -5);
		source.setLayoutData(sourceFormData);

		sourceLabelFormData.top = new FormAttachment(source, 0, SWT.CENTER);

		Label targetLabel = getFormToolkit().createLabel(container, "Target:"); //$NON-NLS-1$
		FormData targetLabelFormData = new FormData();
		targetLabelFormData.left = new FormAttachment(0, 5);
		targetLabel.setLayoutData(targetLabelFormData);

		final Text target = createLeftAlignedTextField(container, "",
				EDPHandlersPackage.Literals.PARAMETERIZED__PARAMETERS,
				new TextToFirstStringParameterBinding("target"), //$NON-NLS-1$
				targetChangeListener);

		FormData targetFormData = new FormData();
		targetFormData.top = new FormAttachment(source, 5);
		targetFormData.left = new FormAttachment(targetLabel, 5);
		targetFormData.right = new FormAttachment(100, -5);
		target.setLayoutData(targetFormData);

		targetLabelFormData.top = new FormAttachment(target, 0, SWT.CENTER);
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
