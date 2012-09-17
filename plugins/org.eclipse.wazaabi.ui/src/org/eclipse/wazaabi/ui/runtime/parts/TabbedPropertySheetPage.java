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

package org.eclipse.wazaabi.ui.runtime.parts;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.TabRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.ui.model.parts.Page;

public class TabbedPropertySheetPage extends PropertySheetPage {

	public TabbedPropertySheetPage() {
		super();
	}

	public TabbedPropertySheetPage(Page modelPage) {
		super(modelPage);
	}

	public TabbedPropertySheetPage(String uri) {
		super(uri);
	}

	public TabbedPropertySheetPage(URI uri) {
		super(uri);
	}

	private Composite leftComposite = null;
	private boolean displayTitle = false;
	private ScrolledComposite scrolledComposite = null;
	private TabbedPropertyList listComposite = null;

	protected TabbedPropertyList getListComposite() {
		return listComposite;
	}

	private Composite mainComposite = null;

	@Override
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

		FormData formData = new FormData();
		formData.left = new FormAttachment(leftComposite, 0);
		formData.right = new FormAttachment(100, 0);
		if (displayTitle) {
			// formData.top = new FormAttachment(title, 0);
		} else {
			formData.top = new FormAttachment(0, 0);
		}
		formData.bottom = new FormAttachment(100, 0);
		scrolledComposite.setLayoutData(formData);

		formData = new FormData();
		formData.left = new FormAttachment(0, 0);
		formData.right = new FormAttachment(scrolledComposite, 0);
		if (displayTitle) {
			// formData.top = new FormAttachment(title, 0);
		} else {
			formData.top = new FormAttachment(0, 0);
		}
		formData.bottom = new FormAttachment(100, 0);
		leftComposite.setLayoutData(formData);
		super.createControl(scrolledComposite);
		scrolledComposite.setContent(getViewer().getControl());
		scrolledComposite.setAlwaysShowScrollBars(false);
		scrolledComposite.setExpandVertical(true);
		scrolledComposite.setExpandHorizontal(true);

		listComposite = new TabbedPropertyList(leftComposite, this);
		formData = new FormData();
		formData.left = new FormAttachment(0, 0);
		formData.right = new FormAttachment(100, 0);
		formData.top = new FormAttachment(0, 0);
		formData.bottom = new FormAttachment(100, 0);
		listComposite.setLayoutData(formData);
	}

	@Override
	public Control getControl() {
		return mainComposite;
	}

	public void selectTab(int tabIndex) {
		if (getSelectedComponent() instanceof Container) {
			StackLayoutRule stackLayoutRule = (StackLayoutRule) getSelectedComponent()
					.getFirstStyleRule("layout",
							CoreStylesPackage.Literals.STACK_LAYOUT_RULE);
			if (stackLayoutRule != null)
				stackLayoutRule.setTop(tabIndex);
		}
	}

	@Override
	protected void updateSelectedComponent(Object input) {
		super.updateSelectedComponent(input);

		TabItem tabItems[] = new TabItem[] {};
		List<String> labels = new ArrayList<String>();
		int topIndex = 0;
		if (getSelectedComponent() instanceof Container) {

			StackLayoutRule stackLayoutRule = (StackLayoutRule) getSelectedComponent()
					.getFirstStyleRule("layout", //$NON-NLS-1$
							CoreStylesPackage.Literals.STACK_LAYOUT_RULE);
			if (stackLayoutRule != null)
				topIndex = stackLayoutRule.getTop();

			for (AbstractComponent child : ((Container) getSelectedComponent())
					.getChildren()) {
				TabRule tabRule = (TabRule) child.getFirstStyleRule(
						"layout-data", CoreStylesPackage.Literals.TAB_RULE); //$NON-NLS-1$
				if (tabRule != null)
					labels.add(tabRule.getLabel() != null ? tabRule.getLabel()
							: ""); //$NON-NLS-1$
			}
		}
		if (!labels.isEmpty()) {
			tabItems = new TabItem[labels.size()];
			for (int i = 0; i < labels.size(); i++)
				tabItems[i] = new TabItem(labels.get(i));
		}
		getListComposite().setElements(tabItems);
		getListComposite().select(topIndex);
	}

}
