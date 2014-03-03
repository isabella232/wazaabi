/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.viewers.PropertySection;

public class TabbedPropertySheetPage {

	private Composite leftComposite = null;
	private boolean displayTitle = false;
	private ScrolledComposite scrolledComposite = null;
	private TabbedPropertyList listComposite = null;

	protected TabbedPropertyList getListComposite() {
		return listComposite;
	}

	private Composite mainComposite = null;

	public void createControl(Composite parent) {
		mainComposite = new Composite(parent, SWT.NO_FOCUS);
		mainComposite.setLayout(new FormLayout());
		if (displayTitle) {
			// title = new TabbedPropertyTitle(mainComposite, factory);
			//
			// FormData data = new FormData();
			// data.left = new FormAttachment(0, 0);
			// data.right = new FormAttachment(100, 0);
			// data.top = new FormAttachment(0, 0);
			// title.setLayoutData(data);
		}

		leftComposite = new Composite(mainComposite, SWT.NO_FOCUS);
		leftComposite.setLayout(new FormLayout());

		scrolledComposite = new ScrolledComposite(mainComposite, SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.NO_FOCUS);

		FormData mainCompositeFormData = new FormData();
		mainCompositeFormData.left = new FormAttachment(leftComposite, 0);
		mainCompositeFormData.right = new FormAttachment(100, 0);
		if (displayTitle) {
			// formData.top = new FormAttachment(title, 0);
		} else {
			mainCompositeFormData.top = new FormAttachment(0, 0);
		}
		mainCompositeFormData.bottom = new FormAttachment(100, 0);
		scrolledComposite.setLayoutData(mainCompositeFormData);

		FormData leftCompositeFormData = new FormData();
		leftCompositeFormData.left = new FormAttachment(0, 0);
		leftCompositeFormData.right = new FormAttachment(scrolledComposite, 0);
		//		leftCompositeFormData.right = new FormAttachment(20, 0);
		if (displayTitle) {
			// formData.top = new FormAttachment(title, 0);
		} else {
			leftCompositeFormData.top = new FormAttachment(0, 0);
		}
		leftCompositeFormData.bottom = new FormAttachment(100, 0);
		leftComposite.setLayoutData(leftCompositeFormData);
		contents = createContents(scrolledComposite);
		scrolledComposite.setContent(getContents());
		scrolledComposite.setAlwaysShowScrollBars(false);
		scrolledComposite.setExpandVertical(true);
		scrolledComposite.setExpandHorizontal(true);

		listComposite = new TabbedPropertyList(leftComposite, this);
		FormData listCompositeFormData = new FormData();
		listCompositeFormData.left = new FormAttachment(0, 0);
		listCompositeFormData.right = new FormAttachment(100, 0);
		listCompositeFormData.top = new FormAttachment(0, 0);
		listCompositeFormData.bottom = new FormAttachment(100, 0);
		listComposite.setLayoutData(listCompositeFormData);
	}

	private Composite contents = null;

	protected Composite getContents() {
		return contents;
	}

	protected Composite createContents(Composite parent) {
		Composite newContents = new Composite(parent, SWT.NONE);
		newContents.setLayout(new StackLayout());
		return newContents;
	}

	public Composite getControl() {
		return mainComposite;
	}

	public void selectTab(int tabIndex) {
		// if (getSelectedComponent() instanceof Container) {
		// StackLayoutRule stackLayoutRule = (StackLayoutRule)
		// getSelectedComponent()
		// .getFirstStyleRule("layout",
		// CoreStylesPackage.Literals.STACK_LAYOUT_RULE);
		// if (stackLayoutRule != null)
		// stackLayoutRule.setTop(tabIndex);
		// }
	}

	public void updateSelectedComponent(Object input) {

		TabItem tabItems[] = new TabItem[] {};
		int topIndex = 0;

		if (needRecreatePropertySections(input,
				Collections.unmodifiableList(propertySections))) {
			disposeAndClearPropertySections();
			List<PropertySection> newSections = createPropertySections(input);
			if (newSections != null)
				propertySections.addAll(newSections);
			for (PropertySection propertySection : propertySections)
				propertySection.createControls(getContents());
			List<String> labels = getTabLabels(input,
					Collections.unmodifiableList(propertySections));
			if (!labels.isEmpty()) {
				tabItems = new TabItem[labels.size()];
				for (int i = 0; i < labels.size(); i++)
					tabItems[i] = new TabItem(labels.get(i));
			}
			getListComposite().setElements(tabItems);
		}
		setPropertySectionsInput(input);

		((StackLayout) getContents().getLayout()).topControl = propertySections
				.get(topIndex).getControl();
		getListComposite().select(topIndex);
	}

	private List<PropertySection> propertySections = new ArrayList<PropertySection>();

	protected List<PropertySection> createPropertySections(Object input) {
		return Collections.emptyList();
	}

	protected boolean needRecreatePropertySections(Object input,
			List<PropertySection> propertySections) {
		return true;
	}

	protected void disposeAndClearPropertySections() {
		for (PropertySection propertySection : propertySections)
			propertySection.dispose();
		propertySections.clear();
	}

	protected void setPropertySectionsInput(Object input) {
		// TODO : all the property section or only the top one ?
		for (PropertySection propertySection : propertySections)
			propertySection.setInput(input);
	}

	protected void setRefreshPropertySections() {
		// TODO : all the property section or only the top one ?
		for (PropertySection propertySection : propertySections)
			propertySection.refresh();
	}

	protected List<String> getTabLabels(Object input,
			List<PropertySection> propertySections) {
		List<String> result = new ArrayList<String>();
		for (PropertySection propertySection : propertySections)
			result.add(propertySection.getLabel() != null ? propertySection
					.getLabel() : ""); //$NON-NLS-1$
		return result;
	}

}
