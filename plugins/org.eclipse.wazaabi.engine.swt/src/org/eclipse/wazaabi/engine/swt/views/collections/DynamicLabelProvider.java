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

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.engine.edp.coderesolution.AbstractCodeDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider;

public class DynamicLabelProvider implements ILabelProvider,
		ITableLabelProvider {

	private AbstractCodeDescriptor.MethodDescriptor getTextMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getColumnTextMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getImageMethodDescriptor = null;
	private AbstractCodeDescriptor.MethodDescriptor getColumnImageMethodDescriptor = null;
	// TODO : very bad and verbose code
	// we should be able to get the codeDescriptor from the methodDescriptor
	private AbstractCodeDescriptor getTextCodeDescriptor = null;
	private AbstractCodeDescriptor getColumnTextCodeDescriptor = null;

	private AbstractCodeDescriptor getImageCodeDescriptor = null;
	private AbstractCodeDescriptor getColumnImageCodeDescriptor = null;

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
								"getText", new String[] { "element" }, new Class[] { Object.class }, String.class); //$NON-NLS-1$ //$NON-NLS-2$
				if (methodDescriptor != null) {
					getTextMethodDescriptor = methodDescriptor;
					getTextCodeDescriptor = codeDescriptor;
				}
				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"getText", new String[] { "element", "columnIndex" }, new Class[] { Object.class, int.class }, String.class); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				if (methodDescriptor != null) {
					getColumnTextMethodDescriptor = methodDescriptor;
					getColumnTextCodeDescriptor = codeDescriptor;
				}

				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"getImage", new String[] { "element" }, new Class[] { Object.class }, Image.class); //$NON-NLS-1$ //$NON-NLS-2$
				if (methodDescriptor != null) {
					getImageMethodDescriptor = methodDescriptor;
					getImageCodeDescriptor = codeDescriptor;
				}
				methodDescriptor = codeDescriptor
						.getMethodDescriptor(
								"getImage", new String[] { "element", "columnIndex" }, new Class[] { Object.class, int.class }, Image.class); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				if (methodDescriptor != null) {
					getColumnImageMethodDescriptor = methodDescriptor;
					getColumnImageCodeDescriptor = codeDescriptor;
				}

			}
		}
	}

	public void addListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public boolean isLabelProperty(Object element, String property) {
		// TODO Auto-generated method stub
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	public Image getColumnImage(Object element, int columnIndex) {
		if (getColumnImageMethodDescriptor != null
				&& getColumnImageCodeDescriptor != null) {
			return (Image) getColumnImageCodeDescriptor.invokeMethod(
					getColumnImageMethodDescriptor, new Object[] { element,
							columnIndex });
		}
		if (columnIndex == 0)
			if (getImageMethodDescriptor != null
					&& getImageCodeDescriptor != null) {
				return (Image) getImageCodeDescriptor.invokeMethod(
						getImageMethodDescriptor, new Object[] { element });
			}
		return null;
	}

	public String getColumnText(Object element, int columnIndex) {
		if (getColumnTextMethodDescriptor != null
				&& getColumnTextCodeDescriptor != null) {
			String result = (String) getColumnTextCodeDescriptor.invokeMethod(
					getColumnTextMethodDescriptor, new Object[] { element,
							columnIndex });
			return result != null ? result : ""; //$NON-NLS-1$
		}
		if (columnIndex == 0)
			if (getTextMethodDescriptor != null
					&& getTextCodeDescriptor != null) {
				String result = (String) getTextCodeDescriptor.invokeMethod(
						getTextMethodDescriptor, new Object[] { element });
				return result != null ? result : ""; //$NON-NLS-1$
			}
		return ""; //$NON-NLS-1$
	}

	public Image getImage(Object element) {
		return getColumnImage(element, 0);
	}

	public String getText(Object element) {
		if (getTextMethodDescriptor != null && getTextCodeDescriptor != null) {
			String result = (String) getTextCodeDescriptor.invokeMethod(
					getTextMethodDescriptor, new Object[] { element });
			return result != null ? result : ""; //$NON-NLS-1$
		}
		return getColumnText(element, 0);
	}
}
