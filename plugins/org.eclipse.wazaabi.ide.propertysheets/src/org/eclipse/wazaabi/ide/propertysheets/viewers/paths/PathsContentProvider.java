/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.viewers.paths;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;

public class PathsContentProvider implements IStructuredContentProvider {

	private final String blankPathForInsertion;

	public PathsContentProvider(String blankEventForInsertion) {
		this.blankPathForInsertion = blankEventForInsertion;
	}

	public void dispose() {
	}

	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof PathSelector) {
			List<String> paths = new ArrayList<String>(
					((PathSelector) inputElement).getPaths());
			paths.add(blankPathForInsertion);
			return paths.toArray();
		}
		return new Object[] {};
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
}
