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

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;

public class PathSelectorContentProvider implements ITreeContentProvider {

	private final SWTCollectionView collectionView;
	private final Hashtable<String, List<String>> selectors;

	public PathSelectorContentProvider(SWTCollectionView collectionView,
			Hashtable<String, List<String>> selectors) {
		this.collectionView = collectionView;
		this.selectors = selectors;
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO : see what to do here
		System.out.println("Input changed : " + newInput);
		// ((Notifier) collectionView.getHost().getTarget()).eNotify(new
		// ENotificationImpl(
		// (InternalEObject) collectionView.getHost(),
		// Notification.SET, CoreWidgetsPackage.COLLECTION__INPUT,
		// oldInput, newInput));
	}

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof EObject) {
			String eClassName = ((EObject) inputElement).eClass().getName();
			List<Object> result = new ArrayList<Object>();
			IPointersEvaluator pointersEvaluator = getCollectionView()
					.getHost().getViewer().getPointersEvaluator();
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
					System.err.println(e.getMessage()); // TODO :
														// log that
				}

			}
			return result.toArray();
		} /*else if (inputElement instanceof List<?>) {
			List<Object> result = new ArrayList<Object>();
			IPointersEvaluator pointersEvaluator = getCollectionView()
					.getHost().getViewer().getPointersEvaluator();
			List<String> paths = getSelectors().get("List");
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
					System.err.println(e.getMessage()); // TODO :
														// log that
				}

			}
			return result.toArray();
		}*/
		return new Object[] {};
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
