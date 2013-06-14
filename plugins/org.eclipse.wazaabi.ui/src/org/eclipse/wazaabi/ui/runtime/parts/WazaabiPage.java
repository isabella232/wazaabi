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
import org.eclipse.wazaabi.engine.edp.adapters.AbstractPathEventAdapter;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.engine.edp.impl.EDPRegistryImpl;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PathEvent;
import org.eclipse.wazaabi.ui.model.parts.Page;

public class WazaabiPage {

	public static final Container BLANK_CONTAINER = CoreWidgetsFactory.eINSTANCE
			.createContainer();

	private AbstractCodeDescriptor.MethodDescriptor getObjectMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getUiIdMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getUiMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getUiURIMethodDescriptor = null;

	private ResourceSet resourceSet = null;

	private AbstractCodeDescriptor selectionProcessorCodeDescriptor = null;

	private HashMap<Object, AbstractComponent> uiIdToUiRegistry = new HashMap<Object, AbstractComponent>();

	private AbstractCodeDescriptor uiSelectorCodeDescriptor = null;

	private SWTControlViewer viewer = null;

	public void createControl(Composite parent) {
		createViewer(parent);
		initializeContent();
		initializeViewer();
	}

	protected void createViewer(Composite parent) {
		setViewer(new SWTControlViewer(parent));
	}

	public void dispose() {
		releaseCodeDescriptor(getSelectionProcessorCodeDescriptor());
		releaseCodeDescriptor(getUiSelectorCodeDescriptor());

		getObjectMethodDescriptor = null;
		getUiIdMethodDescriptor = null;
		getUiMethodDescriptor = null;
		getUiURIMethodDescriptor = null;
		selectionProcessorCodeDescriptor = null;
		uiSelectorCodeDescriptor = null;

		resourceSet = null;

		uiIdToUiRegistry.clear();

		Control ctrl = getControl();
		if (ctrl != null && !ctrl.isDisposed()) {
			ctrl.dispose();
		}
		viewer = null;
	}

	protected void doSetInput(AbstractComponent component, Object input) {
		component.set("input", input); //$NON-NLS-1$
	}

	protected List<AbstractPathEventAdapter> getAllRelatedAbstractPathEventAdapters(
			EObject root, String contextEntry) {
		List<AbstractPathEventAdapter> result = new ArrayList<AbstractPathEventAdapter>();
		TreeIterator<EObject> i = EcoreUtil.getAllContents(root, true);
		while (i.hasNext()) {
			EObject item = i.next();
			if (item instanceof PathEvent
					&& ((PathEvent) item).getPath() != null
					&& ((PathEvent) item).getPath().startsWith(
							"$" + contextEntry))
				for (Adapter adapter : ((PathEvent) item).eAdapters())
					if (adapter instanceof AbstractPathEventAdapter
							&& !result.contains(adapter))
						result.add((AbstractPathEventAdapter) adapter);
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

	protected ResourceSet getResourceSet() {
		if (this.resourceSet == null)
			this.resourceSet = new ResourceSetImpl();
		return this.resourceSet;
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
		return BLANK_CONTAINER;
	}

	protected AbstractCodeDescriptor getUiSelectorCodeDescriptor() {
		return uiSelectorCodeDescriptor;
	}

	protected String getURI(Object uiId) {
		// TODO : here we read the definition from annotations
		// System.out.println("resolving from configuration for "
		// + uiId.getClass());
		return null;
	}

	protected SWTControlViewer getViewer() {
		return viewer;
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

	protected void createContent() {

	}

	protected void initializeContent() {
	}

	protected void initializeViewer() {
	}

	/**
	 * Loads an AbstractComponent from the given URI using this WazaabiPage's
	 * ResourceSet.
	 * 
	 * @param uri
	 * @return
	 */
	protected AbstractComponent loadUI(String uri) {
		if (getResourceSet() == null)
			return null;
		try {
			Resource resource = getResourceSet().getResource(
					URI.createURI(uri), true);
			if (resource != null
					&& !resource.getContents().isEmpty()
					&& resource.getContents().get(0) instanceof AbstractComponent)
				return (AbstractComponent) resource.getContents().get(0);
		} catch (Throwable t) {
			// TODO : log this : the '.ui' has not been found
		}
		return null;
	}

	public void refresh(AbstractComponent component) {
		List<AbstractPathEventAdapter> adapters = getAllRelatedAbstractPathEventAdapters(
				component, "input"); //$NON-NLS-1$
		for (AbstractPathEventAdapter adapter : adapters) {
			adapter.getEventHandlerAdapter().trigger(
					(Event) adapter.getTarget());
		}
	}

	protected void registerSelectionProcessorMethods(
			AbstractCodeDescriptor codeDescriptor) {
		if (codeDescriptor == null)
			return;
		getObjectMethodDescriptor = codeDescriptor
				.getMethodDescriptor(
						"getObject", new String[] { "object" }, new Class[] { Object.class }, Object.class); //$NON-NLS-1$ //$NON-NLS-2$
	}

	protected void registerUIselectorMethods(
			AbstractCodeDescriptor codeDescriptor) {
		if (codeDescriptor == null)
			return;
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
			uri = getURI(uiId);
			if (uri == null || "".equals(uri)) //$NON-NLS-1$
				if (getGetUiURIMethodDescriptor() != null)
					uri = (String) getUiSelectorCodeDescriptor().invokeMethod(
							getGetUiURIMethodDescriptor(),
							new Object[] { input, uiId });
		}

		if (uri != null && !"".equals(uri)) { //$NON-NLS-1$
			ui = EcoreUtil.copy(loadUI(uri)); // we need to duplicate, otherwise
												// the resource will be emptied
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
		List<AbstractPathEventAdapter> adapters = getAllRelatedAbstractPathEventAdapters(
				component, "input"); //$NON-NLS-1$

		doSetInput(component, input);
		for (AbstractPathEventAdapter adapter : adapters) {
			adapter.attachListeners();
			adapter.getEventHandlerAdapter().trigger(
					(Event) adapter.getTarget());
		}
	}

	protected void setResourceSet(ResourceSet resourceSet) {
		this.resourceSet = resourceSet;
	}

	protected void setViewer(SWTControlViewer viewer) {
		this.viewer = viewer;
	}

	protected void unsetInput(AbstractComponent component) {
		if (component == BLANK_CONTAINER)
			return;
		List<AbstractPathEventAdapter> adapters = getAllRelatedAbstractPathEventAdapters(
				component, "input");
		for (AbstractPathEventAdapter adapter : adapters)
			adapter.detachListeners();
	}

}
