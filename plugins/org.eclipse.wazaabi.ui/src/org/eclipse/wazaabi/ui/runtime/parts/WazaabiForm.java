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
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.edp.Registry;
import org.eclipse.wazaabi.engine.edp.adapters.PropertyChangedEventAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.impl.EDPRegistryImpl;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;
import org.eclipse.wazaabi.ui.model.parts.Page;

public class WazaabiForm {

	public static final Container BLANK_CONTAINER = CoreWidgetsFactory.eINSTANCE
			.createContainer();

	private AbstractCodeDescriptor.MethodDescriptor getObjectMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getUiIdMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getUiMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getUiURIMethodDescriptor = null;
	private Page modelPage = null;

	private AbstractCodeDescriptor selectionProcessorCodeDescriptor = null;

	private HashMap<Object, AbstractComponent> uiIdToUiRegistry = new HashMap<Object, AbstractComponent>();

	private AbstractCodeDescriptor uiSelectorCodeDescriptor = null;

	private URI uri;

	private SWTControlViewer viewer = null;

	private ResourceSet resourceSet = null;

	public WazaabiForm(String uri) {
		this(URI.createURI(uri));
	}

	public WazaabiForm(URI uri) {
		this.uri = uri;
		// TODO : test uri != null
	}

	public WazaabiForm(Page modelPage) {
		this.modelPage = modelPage;
	}

	public void createControl(Composite parent) {
		createViewer(parent);
		if (modelPage == null)
			initializeModelPage();
		initializeViewer();
	}

	protected void createViewer(Composite parent) {
		setViewer(new SWTControlViewer(parent));
	}

	protected void initializeViewer() {
		getViewer().setContents(getModelPage());
	}

	protected void initializeModelPage() {
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

	protected void initializeCodeDescriptors(Page page) {
		if (page != null) {
			Registry registry = new EDPRegistryImpl();

			// TODO move from this location
			selectionProcessorCodeDescriptor = (AbstractCodeDescriptor) registry
					.createComponent(this, page.getSelectionProcessor(), null,
							AbstractCodeDescriptor.class);
			registerSelectionProcessorMethods(getSelectionProcessorCodeDescriptor());
			uiSelectorCodeDescriptor = (AbstractCodeDescriptor) registry
					.createComponent(this, page.getUiSelector(), null,
							AbstractCodeDescriptor.class);
			registerUIselectorMethods(getUiSelectorCodeDescriptor());
		}
	}

	public void dispose() {
		releaseCodeDescriptor(getSelectionProcessorCodeDescriptor());
		releaseCodeDescriptor(getUiSelectorCodeDescriptor());
		Control ctrl = getControl();
		if (ctrl != null && !ctrl.isDisposed()) {
			ctrl.dispose();
		}
	}

	protected List<PropertyChangedEventAdapter> getAllRelatedPropertyChangedEventAdapters(
			EObject root, String contextEntry) {
		List<PropertyChangedEventAdapter> result = new ArrayList<PropertyChangedEventAdapter>();
		TreeIterator<EObject> i = EcoreUtil.getAllContents(root, true);
		while (i.hasNext()) {
			EObject item = i.next();
			if (item instanceof PropertyChangedEvent
					&& ((PropertyChangedEvent) item).getPath() != null
					&& ((PropertyChangedEvent) item).getPath().startsWith(
							"$" + contextEntry))
				for (Adapter adapter : ((PropertyChangedEvent) item)
						.eAdapters())
					if (adapter instanceof PropertyChangedEventAdapter
							&& !result.contains(adapter))
						result.add((PropertyChangedEventAdapter) adapter);
		}
		return result;
	}

	public Control getControl() {
		return getViewer().getControl();
	}

	protected AbstractCodeDescriptor.MethodDescriptor getGetObjectMethodDescriptor() {
		return getObjectMethodDescriptor;
	}

	protected AbstractCodeDescriptor.MethodDescriptor getGetUiIdMethodDescriptor() {
		return getUiIdMethodDescriptor;
	}

	protected AbstractCodeDescriptor.MethodDescriptor getGetUiMethodDescriptor() {
		return getUiMethodDescriptor;
	}

	protected AbstractCodeDescriptor.MethodDescriptor getGetUiURIMethodDescriptor() {
		return getUiURIMethodDescriptor;
	}

	protected AbstractCodeDescriptor.MethodDescriptor getReleaseMethodDescriptor(
			AbstractCodeDescriptor codeDescriptor) {
		if (codeDescriptor != null)
			return codeDescriptor.getMethodDescriptor(
					"dispose", null, null, null); //$NON-NLS-1$
		return null;
	}

	protected AbstractCodeDescriptor getSelectionProcessorCodeDescriptor() {
		return selectionProcessorCodeDescriptor;
	}

	protected AbstractComponent getUi(Object input) {
		if (input != null) {
			if (getUiSelectorCodeDescriptor() != null
					&& getGetUiIdMethodDescriptor() != null)
				return resolveUI(
						input,
						getUiSelectorCodeDescriptor().invokeMethod(
								getGetUiIdMethodDescriptor(),
								new Object[] { input }));
			return resolveUI(input, input.getClass());
		}
		return null;
	}

	protected AbstractCodeDescriptor getUiSelectorCodeDescriptor() {
		return uiSelectorCodeDescriptor;
	}

	public URI getUri() {
		return uri;
	}

	protected String getURIFromConfiguration(Object uiId) {
		// TODO : here we read the definition from annotations
		// System.out.println("resolving from configuration for "
		// + uiId.getClass());
		return null;
	}

	protected AbstractComponent loadUI(String uri) {
		if (getModelPage() != null && getModelPage().eResource() != null
				&& getModelPage().eResource().getResourceSet() != null)
			this.resourceSet = getModelPage().eResource().getResourceSet();
		if (this.resourceSet == null)
			this.resourceSet = new ResourceSetImpl();

		try {
			Resource resource = this.resourceSet.getResource(
					URI.createURI(uri), true);
			if (resource != null
					&& !resource.getContents().isEmpty()
					&& resource.getContents().get(0) instanceof AbstractComponent)
				return (AbstractComponent) resource.getContents().get(0);
		} catch (Throwable t) {
			t.printStackTrace();
			// TODO : log this
		}
		return null;
	}

	public void refresh(AbstractComponent component) {
		List<PropertyChangedEventAdapter> adapters = getAllRelatedPropertyChangedEventAdapters(
				component, "input"); //$NON-NLS-1$
		for (PropertyChangedEventAdapter adapter : adapters) {
			adapter.getEventHandlerAdapter().trigger(
					(Event) adapter.getTarget());
		}
	}

	protected void registerSelectionProcessorMethods(
			AbstractCodeDescriptor codeDescriptor) {
		getObjectMethodDescriptor = codeDescriptor
				.getMethodDescriptor(
						"getObject", new String[] { "object" }, new Class[] { Object.class }, Object.class); //$NON-NLS-1$ //$NON-NLS-2$
	}

	protected void registerUIselectorMethods(
			AbstractCodeDescriptor codeDescriptor) {
		getUiMethodDescriptor = codeDescriptor.getMethodDescriptor("getUi",
				new String[] { "element", "uiId" }, new Class[] { Object.class,
						Object.class }, AbstractComponent.class); //$NON-NLS-1$
		getUiIdMethodDescriptor = codeDescriptor.getMethodDescriptor("getUiId",
				new String[] { "element" }, new Class[] { Object.class },
				Object.class); //$NON-NLS-1$
		getUiURIMethodDescriptor = codeDescriptor
				.getMethodDescriptor(
						"getUiURI", new String[] { "element", "uiId" }, new Class[] { Object.class, Object.class }, String.class); //$NON-NLS-1$
	}

	protected void releaseCodeDescriptor(AbstractCodeDescriptor codeDescriptor) {
		if (codeDescriptor != null && codeDescriptor.isInstanciated()) {
			AbstractCodeDescriptor.MethodDescriptor releaseMethodDescriptor = getReleaseMethodDescriptor(codeDescriptor);
			if (releaseMethodDescriptor != null)
				codeDescriptor.invokeMethod(releaseMethodDescriptor, null);
		}
	}

	protected AbstractComponent resolveUI(Object input, Object uiId) {

		AbstractComponent ui = uiIdToUiRegistry.get(uiId);

		if (ui != null)
			return ui;

		String uri = null;

		if (uri == null || "".equals(uri)) { //$NON-NLS-1$ 
			uri = getURIFromConfiguration(uiId);
			if (uri == null || "".equals(uri)) //$NON-NLS-1$
				if (getGetUiURIMethodDescriptor() != null)
					uri = (String) getUiSelectorCodeDescriptor().invokeMethod(
							getGetUiURIMethodDescriptor(),
							new Object[] { input, uiId });
		}

		if (uri != null && !"".equals(uri)) { //$NON-NLS-1$
			ui = loadUI(uri);
			if (ui != null) {
				uiIdToUiRegistry.put(uiId, ui);
				return ui;
			}
		} else if (getGetUiMethodDescriptor() != null)
			ui = (AbstractComponent) getUiSelectorCodeDescriptor()
					.invokeMethod(getGetUiMethodDescriptor(),
							new Object[] { input, uiId });

		if (ui == null)
			ui = BLANK_CONTAINER;
		uiIdToUiRegistry.put(uiId, ui);
		return ui;
	}

	public void setFocus() {
		if (getControl() != null && !getControl().isDisposed())
			getControl().setFocus();
	}

	protected void setInput(AbstractComponent component, Object input) {
		if (component == BLANK_CONTAINER)
			return;
		List<PropertyChangedEventAdapter> adapters = getAllRelatedPropertyChangedEventAdapters(
				component, "input"); //$NON-NLS-1$

		component.set("input", input); //$NON-NLS-1$
		for (PropertyChangedEventAdapter adapter : adapters) {
			adapter.attachListeners();
			adapter.getEventHandlerAdapter().trigger(
					(Event) adapter.getTarget());
		}
	}

	protected void unsetInput(AbstractComponent component) {
		if (component == BLANK_CONTAINER)
			return;
		List<PropertyChangedEventAdapter> adapters = getAllRelatedPropertyChangedEventAdapters(
				component, "input");
		for (PropertyChangedEventAdapter adapter : adapters)
			adapter.detachListeners();
	}

	protected SWTControlViewer getViewer() {
		return viewer;
	}

	protected void setViewer(SWTControlViewer viewer) {
		this.viewer = viewer;
	}

	protected Page getModelPage() {
		return modelPage;
	}

	protected void setModelPage(Page modelPage) {
		this.modelPage = modelPage;
	}

}
