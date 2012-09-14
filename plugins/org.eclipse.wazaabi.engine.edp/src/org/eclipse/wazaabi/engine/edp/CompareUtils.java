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

package org.eclipse.wazaabi.engine.edp;

public class CompareUtils {

	public static boolean areEquals(boolean first, boolean second) {
		return first == second;
	}

	public static boolean areEquals(int first, int second) {
		return first == second;
	}

	public static boolean areEquals(short first, short second) {
		return first == second;
	}

	public static boolean areEquals(String first, String second) {
		if (first == null && second == null)
			return true;
		if (first != null && first.equals(second))
			return true;
		if (second != null && second.equals(first))
			return true;
		return false;
	}

}
