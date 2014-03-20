/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.e4.ui.workbench.renderers.wazaabi.swt;

import java.io.IOException;
import java.net.URL;

import javax.inject.Inject;

import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.core.services.log.Logger;
import org.eclipse.e4.ui.model.application.ui.MUIElement;
import org.eclipse.e4.ui.model.application.ui.basic.MPart;
import org.eclipse.e4.ui.workbench.IPresentationEngine;
import org.eclipse.e4.ui.workbench.modeling.ESelectionService;
import org.eclipse.e4.ui.workbench.renderers.swt.ContributedPartRenderer;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.CoreUtils;
import org.eclipse.wazaabi.engine.edp.adapters.EventHandlerAdapter;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;

@SuppressWarnings("restriction")
public class WazaabiPartRenderer extends ContributedPartRenderer {

	@Inject
	private IPresentationEngine engine;

	@Inject
	ESelectionService selectionService;

	@Optional
	@Inject
	private Logger logger;

	private SWTControlViewer viewer = null;

	public Object createWidget(final MUIElement element, Object parent) {
		if (!(element instanceof MPart) || !(parent instanceof Composite))
			return null;
		Widget parentWidget = (Widget) parent;
		final MPart part = (MPart) element;
		final Composite newComposite = new Composite((Composite) parentWidget,
				SWT.NONE);

		newComposite.setLayout(new FillLayout(SWT.VERTICAL));
		viewer = new SWTControlViewer(newComposite);

		String uri = part.getContributionURI();
		uri = uri.substring("bundleclass://".length());
		int idx = uri.indexOf("/");
		String bundleName = uri.substring(0, idx);
		String resourcePath = uri.substring(idx + 1);

		AbstractComponent root = null;
		Bundle bundle = FrameworkUtil.getBundle(WazaabiPartRenderer.class);
		BundleContext context = bundle.getBundleContext();
		Bundle bb = null;
		for (Bundle b : context.getBundles())
			if (b.getSymbolicName().equals(bundleName)) {
				bb = b;
				break;
			}
		if (bb != null) {
			URL url = bb.getEntry(resourcePath);
			if (url != null) {
				XMIResourceImpl r = new XMIResourceImpl();
				try {
					r.load(url.openStream(), null);
				} catch (IOException e) {
					e.printStackTrace();
				}
				if (r.getContents() != null && !r.getContents().isEmpty()
						&& r.getContents().get(0) instanceof AbstractComponent)
					root = (AbstractComponent) r.getContents().get(0);
			}
		}

		viewer.setContents(root);
		CoreUtils.refresh(root);
		root.set("SelectionService", selectionService);
		for (EventHandler eventHandler : root.getHandlers())
			// TODO : be sure that we handle only "e4:ui:initialize"
			for (Adapter a : eventHandler.eAdapters()) {
				EventHandlerAdapter adapter = (EventHandlerAdapter) a;
				adapter.trigger(root, eventHandler, eventHandler.getEvents()
						.get(0));
			}

		return newComposite;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.e4.ui.workbench.renderers.swt.SWTPartRenderer#requiresFocus
	 * (org.eclipse.e4.ui.model.application.ui.basic.MPart)
	 */
	@Override
	protected boolean requiresFocus(MPart element) {
		// if (element == partToActivate) {
		// return true;
		// }
		return super.requiresFocus(element);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.e4.ui.workbench.renderers.swt.PartFactory#hookControllerLogic
	 * (org.eclipse.e4.ui.model.application.MPart)
	 */
	@Override
	public void hookControllerLogic(final MUIElement me) {
		// super.hookControllerLogic(me);
		// if (!(me instanceof MPart)) {
		// return;
		// }
		// Widget widget = (Widget) me.getWidget();
		// if (widget instanceof Composite) {
		// widget.addListener(SWT.Activate, activationListener);
		// }

	}

	@Override
	public Object getUIContainer(MUIElement element) {
		// if (element instanceof MToolBar) {
		// MUIElement container = (MUIElement) ((EObject) element)
		// .eContainer();
		// MUIElement parent = container.getParent();
		// if (parent == null) {
		// MPlaceholder placeholder = container.getCurSharedRef();
		// if (placeholder != null) {
		// return placeholder.getParent().getWidget();
		// }
		// } else {
		// return parent.getWidget();
		// }
		// }
		return super.getUIContainer(element);
	}

	@Override
	public void disposeWidget(MUIElement element) {
		// if (element instanceof MPart) {
		// MPart part = (MPart) element;
		// MToolBar toolBar = part.getToolbar();
		// if (toolBar != null) {
		// Widget widget = (Widget) toolBar.getWidget();
		// if (widget != null) {
		// unbindWidget(toolBar);
		// widget.dispose();
		// }
		// }
		//
		// for (MMenu menu : part.getMenus()) {
		// engine.removeGui(menu);
		// }
		// }
		super.disposeWidget(element);
	}
}
