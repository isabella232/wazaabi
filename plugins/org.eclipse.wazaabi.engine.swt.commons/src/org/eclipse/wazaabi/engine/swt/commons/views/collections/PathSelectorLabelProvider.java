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

import java.util.Hashtable;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;

public class PathSelectorLabelProvider implements ITableLabelProvider,
		ILabelProvider {

	public PathSelectorLabelProvider(SWTCollectionView collectionView,
			Hashtable<String, List<String>> selectors) {
		this.collectionView = collectionView;
		this.selectors = selectors;
	}

	private final SWTCollectionView collectionView;
	private final Hashtable<String, List<String>> selectors;

	public void removeListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	public boolean isLabelProperty(Object element, String property) {
		// TODO Auto-generated method stub
		return false;
	}

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void addListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	public String getColumnText(Object element, int columnIndex) {
		if (element instanceof EObject) {
			String eClassName = ((EObject) element).eClass().getName();
			List<String> paths = getSelectors().get(eClassName);

			if (paths != null) {
				if (paths.size() > columnIndex) {
					String path = paths.get(columnIndex);
					if (path == null || path.length() == 0)
						return ""; //$NON-NLS-1$
					IPointersEvaluator pointersEvaluator = getCollectionView()
							.getHost().getViewer().getPointersEvaluator();
					List<?> pointers = pointersEvaluator.selectPointers(
							element, path);
					if (pointers.size() > 0) {
						Object value = pointersEvaluator.getValue(pointers
								.get(0));
						if (value instanceof List) {
							if (!((List<?>) value).isEmpty())
								value = ((List<?>) value).get(0);
							else
								value = ""; //$NON-NLS-1$
						}
						if (value != null)
							return value.toString();
					}
				} else
					return ""; //$NON-NLS-1$
			}
		} else if (element instanceof FeatureMap) {
			System.out.println(element);
		}
		return element != null ? element.toString() : "null"; //$NON-NLS-1$
	}

	public Image getColumnImage(Object element, int columnIndex) {
		return null;
	}

	protected SWTCollectionView getCollectionView() {
		return collectionView;
	}

	protected Hashtable<String, List<String>> getSelectors() {
		return selectors;
	}

	public Image getImage(Object element) {
		return getColumnImage(element, 0);
	}

	public String getText(Object element) {
		return getColumnText(element, 0);
	}

}
