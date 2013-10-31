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

package org.eclipse.wazaabi.swt.starterkit;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.engine.core.themes.nonosgi.CoreThemesHelper;
import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.swt.forms.nonosgi.SWTFormsHelper;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Wazaabi {

	final static Logger logger = LoggerFactory.getLogger(Wazaabi.class);

	public static final String DOMAIN_VAR_NAME = "domain"; //$NON-NLS-1$

	public static SWTControlViewer createUI(Composite parent,
			AbstractComponent uiModel, Object domainModel) {
		if (parent == null || parent.isDisposed()) {
			logger.error("invalid parent (null or disposed Composite"); //$NON-NLS-1$
			return null;
		}
		if (uiModel == null) {
			logger.error("uiModel is null"); //$NON-NLS-1$
			return null;
		}
		uiModel.set(DOMAIN_VAR_NAME, domainModel);
		SWTControlViewer viewer = new SWTControlViewer(parent);
		initNonOSGI(viewer);
		viewer.setContents(uiModel);
		return viewer;
	}

	public static SWTControlViewer createUI(Composite parent, String path,
			Object domainModel) {
		if (parent == null || parent.isDisposed()) {
			logger.error("invalid parent (null or disposed Composite"); //$NON-NLS-1$
			return null;
		}
		if (path == null || path.isEmpty()) {
			logger.error("path is null or empty"); //$NON-NLS-1$
			return null;
		}
		XMIResource res = new XMIResourceImpl();

		SWTControlViewer viewer = new SWTControlViewer(parent);
		initNonOSGI(viewer);
		ICodeLocator codeLocator = (ICodeLocator) viewer.getFactoryFor(null,
				path, null, ICodeLocator.class);
		if (codeLocator != null) {
			InputStream in = null;
			try {
				in = codeLocator.getResourceInputStream(path);
			} catch (IOException e) {
				logger.error("Cannot get the resource's inputStream {}", e); //$NON-NLS-1$
			}
			if (in != null) {
				try {
					res.load(in, null);

				} catch (IOException e) {
					logger.error("Cannot load the resource {}", e); //$NON-NLS-1$
				}
				try {
					in.close();
				} catch (IOException e) {
					logger.error("Error while closing {}, {}", path, e); //$NON-NLS-1$
				}
			}
			if (res != null && res.getContents().size() == 1
					&& res.getContents().get(0) instanceof AbstractComponent) {
				((AbstractComponent) res.getContents().get(0)).set(
						DOMAIN_VAR_NAME, domainModel);
				viewer.setContents(res.getContents().get(0));
				return viewer;
			}
		} else {
			// URI uri = new URI()
		}
		return null;
	}

	protected static void initNonOSGI(SWTControlViewer viewer) {
		if (Activator.getContext() != null)
			return;
		SWTFormsHelper.init(viewer);
		SWTHelper.init(viewer);
		URNJavaLocatorHelper.init(viewer);
		LocationPathsHelper.init(viewer);
		CoreThemesHelper.init(viewer);
	}
}
