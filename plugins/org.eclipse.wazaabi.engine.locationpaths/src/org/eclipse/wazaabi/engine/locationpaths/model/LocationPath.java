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

package org.eclipse.wazaabi.engine.locationpaths.model;

import java.util.ArrayList;
import java.util.List;

public class LocationPath extends Expression {

	private List steps = new ArrayList();
	private InitialContext initialContext = null;

	public InitialContext getInitialContext() {
		return initialContext;
	}

	public void setInitialContext(InitialContext initialContext) {
		this.initialContext = initialContext;
	}

	public List getSteps() {
		return steps;
	}
	

}
