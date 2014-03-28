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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.AbstractUIContentsDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.InPlaceCellEditor;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.AbstractBinding;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.bindings.TextToStringBinding;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;

public class PathSelectorCellEditor extends InPlaceCellEditor {

	private FormToolkit formToolkit = null;
	private PathSelectorContentDescriptor contentDescriptor;

	protected static class PathSelectorContentDescriptor extends
			AbstractUIContentsDescriptor {
		private static TextToStringBinding TEXT_TO_STRING_BINDING = new TextToStringBinding();

		private final FormToolkit formToolkit;

		public PathSelectorContentDescriptor(FormToolkit formToolkit) {
			this.formToolkit = formToolkit;
		}

		@Override
		public Object getUniqueID() {
			return this.getClass().getName();
		}

		@Override
		public String getTitle() {
			return "title";
		}

		@Override
		public Control createContents(Control parent,
				TargetChangeListener targetChangeListener) {

			Label eClassifierNameLabel = formToolkit.createLabel(
					(Composite) parent, "Classifier name:");
			FormData eClassifierNameLabelLayoutData = new FormData();
			eClassifierNameLabel.setLayoutData(eClassifierNameLabelLayoutData);
			eClassifierNameLabelLayoutData.left = new FormAttachment(0, 5);

			Text eClassifierName = createTextField(
					(Composite) parent,
					"Classifier name",
					CoreCollectionsStylesPackage.Literals.PATH_SELECTOR__ECLASSIFIER_NAME,
					TEXT_TO_STRING_BINDING, targetChangeListener);
			FormData eClassifierNameLayoutData = new FormData();
			eClassifierName.setLayoutData(eClassifierNameLayoutData);
			eClassifierNameLayoutData.top = new FormAttachment(0, 5);
			eClassifierNameLayoutData.left = new FormAttachment(
					eClassifierNameLabel, 5);
			eClassifierNameLayoutData.right = new FormAttachment(100, -5);

			eClassifierNameLabelLayoutData.top = new FormAttachment(
					eClassifierName, 5, SWT.CENTER);

			return null;
		}

		protected Text createTextField(Composite parent, String text,
				EStructuralFeature feature, AbstractBinding binding,
				TargetChangeListener targetChangeListener) {
			Text field = formToolkit.createText(parent, "", SWT.RIGHT
					| SWT.BORDER);
			bind(field, binding, feature, targetChangeListener);
			return field;
		}
	};

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

		contentDescriptor = new PathSelectorContentDescriptor(getFormToolkit());
		contentDescriptor.createContents(form.getBody(), this);

		return form;
	}

	protected FormToolkit getFormToolkit() {
		return formToolkit;
	}

	protected String getHeaderTitle() {
		return "Path Selector";
	}

	@Override
	protected void setInput(Object input) {
		super.setInput(input);
		contentDescriptor.setInput(getControl(), (EObject) input);
	}

	@Override
	protected void doSetFocus() {
		super.doSetFocus();
		// if (eventsTableViewer != null && eventsTableViewer.getControl() !=
		// null
		// && !eventsTableViewer.getControl().isDisposed())
		// eventsTableViewer.getControl().setFocus();
	}

	@Override
	public void refresh() {
		contentDescriptor.refresh(getControl());
		super.refresh();
	}
}
