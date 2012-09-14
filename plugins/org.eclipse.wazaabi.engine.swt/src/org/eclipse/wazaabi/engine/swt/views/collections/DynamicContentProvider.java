/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.views.collections;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;

public class DynamicContentProvider implements IStructuredContentProvider {

	private static final Object[] EMPTY_ARRAY = {};

	private AbstractCodeDescriptor.MethodDescriptor getChildrenMethodDescriptor = null;
	// TODO : very bad and verbose code
	// we should be able to get the codeDescriptor from the methodDescriptor
	private AbstractCodeDescriptor getChildrenCodeDescriptor = null;

	public void updateDynamicProviderURIs(List<String> uris) {
		for (String uri : uris) {
			AbstractCodeDescriptor codeDescriptor = EDPSingletons
					.getComposedCodeLocator().resolveCodeDescriptor(uri);
			if (codeDescriptor != null) {
				AbstractCodeDescriptor.MethodDescriptor methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"getChildren", new String[] { "parent" }, new Class[] { Object.class }, List.class); //$NON-NLS-1$ 
				if (methodDescriptor != null) {
					getChildrenMethodDescriptor = methodDescriptor;
					getChildrenCodeDescriptor = codeDescriptor;
				}
			}
		}
	}

	
	public void dispose() {
		// TODO Auto-generated method stub

	}

	
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO Auto-generated method stub

	}

	
	public Object[] getElements(Object inputElement) {
		if (getChildrenMethodDescriptor != null
				&& getChildrenCodeDescriptor != null) {
			List<?> children = (List<?>) getChildrenCodeDescriptor
					.invokeMethod(getChildrenMethodDescriptor,
							new Object[] { inputElement });
			if (children != null)
				return children.toArray();
		}
		return EMPTY_ARRAY;
	}
}
