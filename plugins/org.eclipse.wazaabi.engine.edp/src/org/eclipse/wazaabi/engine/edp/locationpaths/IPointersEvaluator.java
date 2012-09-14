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

package org.eclipse.wazaabi.engine.edp.locationpaths;

import java.util.List;

public interface IPointersEvaluator {

	public List<?> selectPointers(Object context, String path);

	public String getPropertyName(Object pointer);

	public Object getContext(Object pointer);

	/**
	 * Evaluates and returns the value associated to this pointer.
	 * 
	 * @param pointer
	 * @return
	 */
	public Object getValue(Object pointer);

	/**
	 * Set the pointer with the given value.
	 * 
	 * @param pointer
	 * @param newValue
	 */
	public void setValue(Object pointer, Object newValue, IConverter converter);
	public void setValue2(Object pointer, Object newValue);

	public IConverter resolveConverter(Object pointer, Object newValue);

}
