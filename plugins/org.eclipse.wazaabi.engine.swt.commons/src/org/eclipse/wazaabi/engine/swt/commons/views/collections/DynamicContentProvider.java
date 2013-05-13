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

package org.eclipse.wazaabi.engine.swt.commons.views.collections;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider;

public class DynamicContentProvider implements IStructuredContentProvider,
		ITreeContentProvider {

	private static final Object[] EMPTY_ARRAY = {};

	private AbstractCodeDescriptor.MethodDescriptor getChildrenMethodDescriptor = null;
	// TODO : very bad and verbose code
	// we should be able to get the codeDescriptor from the methodDescriptor
	private AbstractCodeDescriptor getChildrenCodeDescriptor = null;

	private AbstractCodeDescriptor.MethodDescriptor getParentMethodDescriptor = null;
	private AbstractCodeDescriptor getParentCodeDescriptor = null;

	private AbstractCodeDescriptor.MethodDescriptor hasChildrenMethodDescriptor = null;
	private AbstractCodeDescriptor hasChildrenCodeDescriptor = null;

	public DynamicContentProvider() {
		System.out.println("create Dynamic content provider");
	}

	public void updateDynamicProviderURIs(
			List<DynamicProvider> dynamicProviders, String baseURI) {
		for (DynamicProvider dynamicProvider : dynamicProviders) {
			String uri = dynamicProvider.getUri();
			if (baseURI != null && !baseURI.isEmpty())
				uri = EDPSingletons.getComposedCodeLocator().getFullPath(
						baseURI, uri, dynamicProvider);
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
				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"getParent", new String[] { "element" }, new Class[] { Object.class }, Object.class); //$NON-NLS-1$ 
				if (methodDescriptor != null) {
					getParentMethodDescriptor = methodDescriptor;
					getParentCodeDescriptor = codeDescriptor;
				}
				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"hasChildren", new String[] { "element" }, new Class[] { Object.class }, Boolean.class); //$NON-NLS-1$ 
				if (methodDescriptor != null) {
					hasChildrenMethodDescriptor = methodDescriptor;
					hasChildrenCodeDescriptor = codeDescriptor;
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

	public Object[] getChildren(Object parentElement) {
		return getElements(parentElement);
	}

	public Object getParent(Object element) {
		if (getParentMethodDescriptor != null
				&& getParentCodeDescriptor != null) {
			return getParentCodeDescriptor.invokeMethod(
					getParentMethodDescriptor, new Object[] { element });
		}
		return null;
	}

	public boolean hasChildren(Object element) {
		if (hasChildrenMethodDescriptor != null
				&& hasChildrenCodeDescriptor != null) {
			return (Boolean) hasChildrenCodeDescriptor.invokeMethod(
					hasChildrenMethodDescriptor, new Object[] { element });
		}
		return false;
	}
}
