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

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.wazaabi.engine.core.editparts.ContainerEditPart;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.ui.model.parts.Page;

public class PropertySheetPage extends WazaabiPage implements
		IPropertySheetPage, IAdaptable {

	private Page modelPage = null;
	private URI uri;

	public PropertySheetPage() {
	}

	public PropertySheetPage(String uri) {
		this(URI.createURI(uri));
	}

	public PropertySheetPage(URI uri) {
		if (uri == null)
			throw new NullPointerException("URI cannot be null"); //$NON-NLS-1$
		this.uri = uri;
	}

	public PropertySheetPage(Page modelPage) {
		this.modelPage = modelPage;
		initializeCodeDescriptors(modelPage);
	}

	@SuppressWarnings("rawtypes")
	public Object getAdapter(Class adapter) {
		return null;
	}

	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
		Object input = null;

		// TODO : when the selection has IPropertySource, transform property
		// sources into relevant objects
		if (selection instanceof StructuredSelection) {
			StructuredSelection structuredSelection = (StructuredSelection) selection;
			if (structuredSelection.size() == 1)
				if (getSelectionProcessorCodeDescriptor() != null
						&& getGetObjectMethodDescriptor() != null)
					input = getSelectionProcessorCodeDescriptor().invokeMethod(
							getGetObjectMethodDescriptor(),
							new Object[] { structuredSelection
									.getFirstElement() });
				else
					input = structuredSelection.getFirstElement();
			else {
				List<Object> transformedObjects = new ArrayList<Object>();
				for (Object item : structuredSelection.toArray()) {
					if (getSelectionProcessorCodeDescriptor() != null
							&& getGetObjectMethodDescriptor() != null)
						input = getSelectionProcessorCodeDescriptor()
								.invokeMethod(getGetObjectMethodDescriptor(),
										new Object[] { item });
					else
						input = item;
					transformedObjects.add(input);
				}
				input = transformedObjects;
			}
		}
		if (getSelectedComponent() != null)
			unsetInput(getSelectedComponent());
		updateSelectedComponent(input);
		if (getSelectedComponent() != null)
			setInput(getSelectedComponent(), input);
	}

	protected void updateSelectedComponent(Object input) {
		AbstractComponent ui = getUi(input);
		if (ui != BLANK_CONTAINER) {
			for (AbstractComponent child : getModelPage().getChildren())
				if (ui == child) {
					setSelectedComponent(ui);
					return;
				}
			getModelPage().getChildren().add(ui);
			setSelectedComponent(ui);
		} else
			setSelectedComponent(null);
	}

	protected AbstractComponent getSelectedComponent() {
		StackLayoutRule stackLayoutRule = getStackLayoutRule();
		if (stackLayoutRule != null && stackLayoutRule.getTop() != -1)
			return getModelPage().getChildren().get(stackLayoutRule.getTop());
		return null;
	}

	protected void setSelectedComponent(AbstractComponent component) {
		StackLayoutRule stackLayoutRule = getStackLayoutRule();
		// If no StackLayout found, we add a new one
		if (stackLayoutRule == null) {
			stackLayoutRule = CoreStylesFactory.eINSTANCE
					.createStackLayoutRule();
			stackLayoutRule
					.setPropertyName(ContainerEditPart.LAYOUT_PROPERTY_NAME);
			getModelPage().getStyleRules().add(stackLayoutRule);
		}
		if (component == null && stackLayoutRule.getTop() != -1) {
			stackLayoutRule.setTop(-1);
			return;
		}
		if (stackLayoutRule.getTop() != getModelPage().getChildren().indexOf(
				component))
			stackLayoutRule.setTop(getModelPage().getChildren().indexOf(
					component));
	}

	public void setActionBars(IActionBars actionBars) {

	}

	protected StackLayoutRule getStackLayoutRule() {
		// Do we have a StackLayout ?
		for (StyleRule rule : getModelPage().getStyleRules())
			if (ContainerEditPart.LAYOUT_PROPERTY_NAME.equals(rule
					.getPropertyName()) && rule instanceof StackLayoutRule) {
				return (StackLayoutRule) rule;
			}
		return null;
	}

	@Override
	public void dispose() {
		if (getSelectedComponent() != null)
			unsetInput(getSelectedComponent());
		setModelPage(null);
		super.dispose();
	}

	protected Page getModelPage() {
		return modelPage;
	}

	protected void setModelPage(Page modelPage) {
		this.modelPage = modelPage;
	}

	protected void createContent() {

	}

	protected void initializeContent() {
		if (getModelPage() != null)
			return;
		if (getUri() != null) {
			Resource resource = null;
			try {
				resource = new ResourceSetImpl().getResource(getUri(), true); //$NON-NLS-1$
			} catch (Throwable t) {
				// TODO : log this
				t.printStackTrace();
			}
			if (resource != null && !resource.getContents().isEmpty()
					&& resource.getContents().get(0) instanceof Page) {
				setModelPage((Page) resource.getContents().get(0));
				initializeCodeDescriptors((Page) resource.getContents().get(0));
			}
		}// else log : no propertypage found
	}

	protected ResourceSet getResourceSet() {
		if (getModelPage() != null && getModelPage().eResource() != null
				&& getModelPage().eResource().getResourceSet() != null)
			return getModelPage().eResource().getResourceSet();
		else
			return super.getResourceSet();
	}

	protected void initializeViewer() {
		getViewer().setContents(getModelPage());
	}

	public URI getUri() {
		return uri;
	}

}
