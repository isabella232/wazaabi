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

package org.eclipse.wazaabi.engine.swt.snippets.providers;

import org.eclipse.emf.ecore.EPackage;

public class PackagesReverseSorter {

	public int compare(Object element1, Object element2) {
		if (element1 instanceof EPackage && element2 instanceof EPackage) {
			return -((EPackage) element1).getName().compareTo(
					((EPackage) element2).getName());
		}
		return 0;
	}

}
