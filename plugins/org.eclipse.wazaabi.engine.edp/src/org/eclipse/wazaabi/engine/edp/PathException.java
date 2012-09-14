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

public class PathException extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4499411751667865907L;

	public PathException() {
		super();
	}

	public PathException(String message, Throwable throwable) {
		super(message, throwable);
	}

	public PathException(String message) {
		super(message);
	}

	public PathException(Throwable throwable) {
		super(throwable);
	}

}
