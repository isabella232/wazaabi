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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.forms.widgets.ColumnLayout;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.PlaceHolderRuleCellEditor;
import org.eclipse.wazaabi.ide.propertysheets.forms.table.editinghelpers.complexcelleditors.details.FormBasedUIContentsDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.table.ContentProvider;
import org.eclipse.wazaabi.ide.propertysheets.table.editinghelpers.complexcelleditors.details.AbstractUIContentsDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.table.editinghelpers.complexcelleditors.details.UIContentsDescriptorFactory;

public abstract class FormBasedPlaceHolderRuleCellEditor extends
		PlaceHolderRuleCellEditor {

	private Section detailsSection;
	private FormToolkit formToolkit;

	public FormBasedPlaceHolderRuleCellEditor(Composite parent) {
		super(parent);
	}

	protected Control createDetailsSection(Composite parent) {
		Form form = (Form) parent;
		detailsSection = getFormToolkit().createSection(
				form.getBody(),
				Section.DESCRIPTION | Section.TITLE_BAR | Section.TWISTIE
						| Section.EXPANDED);
		detailsSection.setText("Section title");
		detailsSection.setDescription("This is the description that goes "
				+ "below the title");
		detailsSection.setClient(createEmptyDetailsContents(detailsSection));
		return detailsSection;
	}

	protected Composite createEmptyDetailsContents(Composite parent) {
		return getFormToolkit().createComposite(detailsSection);
	}

	protected Layout createLayout() {
		return new ColumnLayout();
	}

	protected Composite createMainSection(Composite parent) {
		formToolkit = new FormToolkit(parent.getDisplay());
		Form form = formToolkit.createForm(parent);
		form.setText(getHeaderTitle());
		formToolkit.decorateFormHeading(form);

		form.getToolBarManager().add(new CloseAction());
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
				return "";
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
		Section section = getFormToolkit().createSection(
				form.getBody(),
				Section.DESCRIPTION | Section.TITLE_BAR | Section.TWISTIE
						| Section.EXPANDED);
		section.setText("Section title");
		section.setDescription("This is the description that goes "
				+ "below the title");
		Composite sectionClient = getFormToolkit().createComposite(section);
		sectionClient.setLayout(new GridLayout());
		setSelectionViewer(createSelectionViewer(sectionClient));
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.grabExcessHorizontalSpace = true;
		getSelectionViewer().getControl().setLayoutData(gridData);
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

	protected Control getDetailsSection() {
		return detailsSection.getClient();
	}

	public FormToolkit getFormToolkit() {
		return formToolkit;
	}

	protected void refreshDetails(
			AbstractUIContentsDescriptor contentsDescriptor) {
		if (detailsSection.getClient() != null
				&& !detailsSection.getClient().isDisposed())
			detailsSection.getClient().dispose();
		Control newContents = null;
		if (contentsDescriptor != null) {
			newContents = contentsDescriptor.createContents(detailsSection,
					this);
		}
		detailsSection.setClient(newContents != null ? newContents
				: createEmptyDetailsContents(detailsSection));
		detailsSection.layout();
	}
}
