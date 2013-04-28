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

package org.eclipse.wazaabi.locationpaths.model;

import java.util.ArrayList;
import java.util.List;

public class Step {

	private int axis = Axis.CHILD;
	private String nameTest = null;
	private List predicates = new ArrayList();

	public int getAxis() {
		return axis;
	}

	public String getNameTest() {
		return nameTest;
	}

	public List getPredicates() {
		return predicates;
	}

	public void setAxis(int axis) {
		this.axis = axis;
	}

	public void setNameTest(String nameTest) {
		this.nameTest = nameTest;
	}

}
