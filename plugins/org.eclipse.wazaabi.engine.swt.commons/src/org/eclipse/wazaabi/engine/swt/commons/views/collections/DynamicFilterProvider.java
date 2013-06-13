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

package org.eclipse.wazaabi.engine.swt.commons.views.collections;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.wazaabi.engine.core.gef.EditPartViewer;
import org.eclipse.wazaabi.engine.edp.EDPUtils;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DynamicFilterProvider extends ViewerFilter {

	private static final Logger logger = LoggerFactory
			.getLogger(DynamicFilterProvider.class);

	private String uri = null;

	private AbstractCodeDescriptor.MethodDescriptor getSelectMethodDescriptor = null;
	// TODO : very bad and verbose code
	// we should be able to get the codeDescriptor from the methodDescriptor

	private AbstractCodeDescriptor getSelectCodeDescriptor = null;

	public String getURI() {
		return uri;
	}

	public void updateDynamicProviderURI(String uri,
			EditPartViewer editPartViewer) {
		this.uri = uri;
		if (uri == null || uri.length() == 0) {
			return;
		}
		String baseURI = editPartViewer.getCodeLocatorBaseUri();
		if (baseURI != null && baseURI.length() != 0)
			uri = EDPUtils.normalizeURI(baseURI, uri);
		AbstractCodeDescriptor codeDescriptor = (AbstractCodeDescriptor) editPartViewer
				.createComponent(this, uri, null, AbstractCodeDescriptor.class);
		if (codeDescriptor != null) {
			AbstractCodeDescriptor.MethodDescriptor methodDescriptor = codeDescriptor
					.getMethodDescriptor(
							"select", new String[] { "parentElement", "element" }, new Class[] { Object.class, Object.class }, boolean.class); //$NON-NLS-1$ //$NON-NLS-2$
			if (methodDescriptor != null) {
				getSelectMethodDescriptor = methodDescriptor;
				getSelectCodeDescriptor = codeDescriptor;
			}
		}
	}

	public void dispose() {
		logger.debug("Disposing filter {}", this);
	}

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (getSelectMethodDescriptor != null
				&& getSelectCodeDescriptor != null) {
			return (Boolean) getSelectCodeDescriptor.invokeMethod(
					getSelectMethodDescriptor, new Object[] { parentElement,
							element });
		}
		return false;
	}
}
