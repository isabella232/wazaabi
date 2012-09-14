/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.edp.tests;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

public final class EMFUtils {

	/**
	 * Returns the number of Adapters derived from the given type attached to
	 * this notifier.
	 * 
	 * @param notifier
	 * @param clazz
	 * @return
	 */
	public static int coundAdaptersOfTypeFor(Notifier notifier, Class<?> clazz) {
		int count = 0;
		for (Adapter item : notifier.eAdapters())
			if (clazz.isAssignableFrom(item.getClass()))
				count++;
		return count;
	}

	/**
	 * Returns Adapter derived from the given type attached to this notifier.
	 * 
	 * @param notifier
	 * @param clazz
	 * @return
	 */
	public static Adapter getAdapterOfTypeFor(Notifier notifier, Class<?> clazz) {
		for (Adapter adapter : notifier.eAdapters())
			if (clazz.isAssignableFrom(adapter.getClass()))
				return adapter;
		return null;
	}
}
