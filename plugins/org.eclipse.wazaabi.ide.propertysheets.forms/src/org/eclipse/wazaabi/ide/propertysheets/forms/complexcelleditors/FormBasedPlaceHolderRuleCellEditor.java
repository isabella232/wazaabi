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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.AbstractListViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CCombo;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.forms.widgets.ColumnLayout;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.AbstractUIContentsDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.PlaceHolderRuleCellEditor;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.UIContentsDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.viewers.ContentProvider;

public abstract class FormBasedPlaceHolderRuleCellEditor extends
		PlaceHolderRuleCellEditor {

	private FormToolkit formToolkit;

	public FormBasedPlaceHolderRuleCellEditor(Composite parent) {
		super(parent);
	}

	protected Control createDetailsSection(Composite parent) {
		Form form = (Form) parent;
		Section detailsSection = getFormToolkit().createSection(form.getBody(),
				Section.TITLE_BAR | Section.TWISTIE | Section.EXPANDED);
		detailsSection.setClient(createEmptyDetailsContents(detailsSection));
		detailsSection.addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				System.out.println("disposed");
			}
		});
		return detailsSection;
	}

	protected Composite createEmptyDetailsContents(Composite parent) {
		return getFormToolkit().createComposite(parent);
	}

	protected Layout createLayout() {
		return new ColumnLayout();
	}

	protected Composite createMainSection(Composite parent) {
		formToolkit = new FormToolkit(parent.getDisplay());
		Form form = formToolkit.createForm(parent);
		form.setText(getHeaderTitle());
		formToolkit.decorateFormHeading(form);

		form.getToolBarManager().add(createCloseAction());
		form.getToolBarManager().update(true);

		form.getBody().setLayout(createLayout());
		return form;
	}

	protected AbstractListViewer createSelectionViewer(Composite parent) {
		CCombo selectionCombo = new CCombo(parent, SWT.BORDER | SWT.DROP_DOWN
				| SWT.READ_ONLY);
		ComboViewer ruleSelectionViewer = new ComboViewer(selectionCombo);
		ruleSelectionViewer.setContentProvider(new ContentProvider() {

			@Override
			public Object[] getElements(Object inputElement) {
				if (inputElement instanceof StyleRuleDescriptor) {
					List<StyleRuleDescriptor> elements = new ArrayList<StyleRuleDescriptor>(
							((StyleRuleDescriptor) inputElement).getChildren());
					elements.add(0, EMPTY_STYLE_RULE_DESCRIPTOR);
					return elements.toArray();
				}
				return new Object[] {};
			}
		});
		ruleSelectionViewer.setLabelProvider(new LabelProvider() {

			@Override
			public String getText(Object element) {
				if (element instanceof StyleRuleDescriptor)
					return ((StyleRuleDescriptor) element).getLabel();
				return ""; //$NON-NLS-1$
			}
		});
		ruleSelectionViewer
				.addSelectionChangedListener(new ISelectionChangedListener() {
					public void selectionChanged(SelectionChangedEvent event) {
						fireSelectionChanged(event);
					}
				});
		return ruleSelectionViewer;
	}

	protected Control createSelectorSection(Composite parent) {
		Form form = (Form) parent;
		Section section = getFormToolkit().createSection(form.getBody(),
				Section.TITLE_BAR | Section.TWISTIE | Section.EXPANDED);
		section.setText(getSelectorSectionTitle());

		Composite sectionClient = getFormToolkit().createComposite(section);
		sectionClient.setLayout(new FormLayout());
		setSelectionViewer(createSelectionViewer(sectionClient));

		FormData formData = new FormData();
		getSelectionViewer().getControl().setLayoutData(formData);
		formData.top = new FormAttachment(0, 5);
		formData.left = new FormAttachment(0, 5);
		formData.right = new FormAttachment(100, -5);
		section.setClient(sectionClient);
		return section;
	}

	protected UIContentsDescriptorFactory createUIContentsDescriptorFactory() {
		return new FormBasedUIContentsDescriptorFactory();
	}

	@Override
	public void dispose() {
		if (formToolkit != null)
			formToolkit.dispose();
		super.dispose();
	}

	protected Section getDetailsSection() {
		return (Section) getDetailControl();
	}

	public FormToolkit getFormToolkit() {
		return formToolkit;
	}

	protected void refreshDetails(
			AbstractUIContentsDescriptor contentsDescriptor) {
		if (getDetailsSection().getClient() != null
				&& !getDetailsSection().getClient().isDisposed())
			getDetailsSection().getClient().dispose();
		Control newContents = null;
		if (contentsDescriptor != null)
			newContents = contentsDescriptor.createContents(
					getDetailsSection(), this);
		getDetailsSection().setClient(
				newContents != null ? newContents
						: createEmptyDetailsContents(getDetailsSection()));
		getDetailsSection()
				.setText(
						contentsDescriptor != null ? contentsDescriptor
								.getTitle() : ""); //$NON-NLS-1$;

		getDetailsSection().layout(true, true);
	}

	protected abstract String getSelectorSectionTitle();
}
