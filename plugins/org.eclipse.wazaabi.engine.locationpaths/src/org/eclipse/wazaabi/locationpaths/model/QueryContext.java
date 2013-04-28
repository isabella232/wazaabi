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

import java.util.Collections;
import java.util.List;

import org.eclipse.wazaabi.engine.locationpaths.runtime.QueryContextResolver;

public class QueryContext implements InitialContext {

	private String functionName = null;
	private List args = Collections.emptyList();

	public void setArgs(List args) {
		this.args = args;
	}

	public String getFunctionName() {
		return functionName;
	}

	public void setFunctionName(String functionName) {
		this.functionName = functionName;
	}

	public List getArgs() {
		return args;
	}

	public Object resolveContext() {
		return QueryContextResolver.resolveInitialContext(getFunctionName(),
				getArgs());
	}

}
