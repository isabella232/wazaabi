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

package org.eclipse.wazaabi.engine.edp.exceptions;

import org.eclipse.wazaabi.engine.edp.adapters.OperationAdapter;

public class OperationAborted extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String errorMsg = "";

	public OperationAborted(OperationAdapter operation) {
		errorMsg = operation.getErrorMessage();
	}

	public OperationAborted(String message) {
		errorMsg = message;
	}

	public OperationAborted(Throwable cause) {
		super(cause);
	}

	public OperationAborted(String message, Throwable cause) {
		super(message, cause);
	}

	public String getErrorMessage() {
		return errorMsg;
	}
}
