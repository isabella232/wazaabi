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

import java.io.File;

public class FilesReverseComparator {

	public int compare(Object element1, Object element2) {
		if (element1 instanceof File && element2 instanceof File) {
			return -((File) element1).getName().compareTo(
					((File) element2).getName());
		}
		return 0;
	}

}
