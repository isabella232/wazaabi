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

public abstract class Pointer<T extends Object> {

	private T context;
	private Step step;

	public T getContext() {
		return context;
	}

	public Step getStep() {
		return step;
	}

	public void setContext(T context) {
		this.context = context;
	}

	public void setStep(Step step) {
		this.step = step;
	}

}
