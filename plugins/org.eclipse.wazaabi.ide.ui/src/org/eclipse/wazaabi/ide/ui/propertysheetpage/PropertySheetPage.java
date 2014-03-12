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

package org.eclipse.wazaabi.ide.ui.propertysheetpage;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.wazaabi.ide.propertysheets.PropertySection;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.EditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.forms.editinghelpers.EventHandlerEditingHelper;
import org.eclipse.wazaabi.ide.propertysheets.forms.editinghelpers.FormBasedEditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedBindingsViewer;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedEventHandlerViewer;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedPropertyTableViewer;
import org.eclipse.wazaabi.ide.propertysheets.forms.viewers.FormBasedStyleRuleTableViewer;
import org.eclipse.wazaabi.ide.propertysheets.tabbed.TabbedPropertySheetPage;
import org.eclipse.wazaabi.ide.ui.editors.WazaabiTreeEditor;
import org.eclipse.wazaabi.ide.ui.editparts.AbstractTreeEditPart;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;

public class PropertySheetPage extends TabbedPropertySheetPage implements
		IPropertySheetPage {

	private WazaabiTreeEditor currentWazaabiTreeEditor = null;

	@Override
	public void setActionBars(IActionBars actionBars) {
	}

	@Override
	public void selectionChanged(IWorkbenchPart part, ISelection selection) {

		Object selectedElement = null;
		if (((StructuredSelection) selection).getFirstElement() instanceof AbstractTreeEditPart)
			selectedElement = ((AbstractTreeEditPart) ((StructuredSelection) selection)
					.getFirstElement()).getModel();
		if (part != currentWazaabiTreeEditor) {
			if (currentWazaabiTreeEditor != null)
				removeTargetChangeListener(currentWazaabiTreeEditor);
			currentWazaabiTreeEditor = null;
		}
		boolean rebuild = buildUI(selectedElement);
		if (rebuild)
			addTargetChangeListener(currentWazaabiTreeEditor);
		setInput(selectedElement);

		if (currentWazaabiTreeEditor != part
				&& part instanceof WazaabiTreeEditor)
			currentWazaabiTreeEditor = (WazaabiTreeEditor) part;
	}

	@Override
	protected List<PropertySection> createPropertySections(Object input) {
		List<PropertySection> result = new ArrayList<PropertySection>();
		if (input instanceof AbstractComponent) {
			result.add(new FormBasedPropertyTableViewer());
			result.add(new FormBasedStyleRuleTableViewer());
			result.add(new FormBasedBindingsViewer());
			result.add(new FormBasedEventHandlerViewer() {

				@Override
				protected EditingHelperFactory createEditingHelperFactory() {
					return new FormBasedEditingHelperFactory() {

						@Override
						protected EventHandlerEditingHelper createEventHandlerEditingHelper() {
							return new EventHandlerEditingHelper(
									new EventHandlerLocator());
						}

					};
				}
			});
		} else
			disposeAndClearPropertySections();
		return result;
	}

	@Override
	protected boolean needRecreatePropertySections(Object input,
			List<PropertySection> propertySections) {
		if (input instanceof AbstractComponent)
			return propertySections.size() != 4
					|| (propertySections.size() == 4 && !(propertySections
							.get(0) instanceof FormBasedPropertyTableViewer
							&& propertySections.get(1) instanceof FormBasedStyleRuleTableViewer
							&& propertySections.get(2) instanceof FormBasedBindingsViewer && propertySections
								.get(3) instanceof FormBasedEventHandlerViewer));
		return true;
	}

}
