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

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PathSelectorContentProvider implements ITreeContentProvider {

	private final SWTCollectionView collectionView;
	private final Hashtable<String, List<String>> selectors;

	private final Logger logger = LoggerFactory
			.getLogger(PathSelectorContentProvider.class);

	public PathSelectorContentProvider(SWTCollectionView collectionView,
			Hashtable<String, List<String>> selectors) {
		this.collectionView = collectionView;
		this.selectors = selectors;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO : see what to do here
		logger.debug("Input changed : {}", newInput);
	}

	public void dispose() {
	}

	public Object[] getElements(Object inputElement) {
		String eClassName = null;
		if (inputElement instanceof EObject)
			eClassName = ((EObject) inputElement).eClass().getName();
		else if (inputElement instanceof List<?>)
			eClassName = "[]"; //$NON-NLS-1$
		else
			return new Object[] {};

		List<Object> result = new ArrayList<Object>();
		IPointersEvaluator pointersEvaluator = getCollectionView().getHost()
				.getViewer().getPointersEvaluator();
		List<String> paths = getSelectors().get(eClassName);
		if (paths == null)
			return new Object[] {};
		for (String path : paths) {
			try {
				List<?> pointers = pointersEvaluator.selectPointers(
						inputElement, path);
				for (Object pointer : pointers) {
					Object value = pointersEvaluator.getValue(pointer);
					if (value instanceof List)
						result.addAll((List<?>) value);
					else
						result.add(value);
				}
			} catch (PathException e) {
				logger.error(e.getMessage());
			}
		}
		return result.toArray();
	}

	protected SWTCollectionView getCollectionView() {
		return collectionView;
	}

	protected Hashtable<String, List<String>> getSelectors() {
		return selectors;
	}

	public Object[] getChildren(Object parentElement) {
		return getElements(parentElement);
	}

	public Object getParent(Object element) {
		return null;
	}

	public boolean hasChildren(Object element) {
		// TODO : we calculate the whole returned array of getElements.
		// there is probably a way to improve that
		return getElements(element).length != 0;
	}

}
