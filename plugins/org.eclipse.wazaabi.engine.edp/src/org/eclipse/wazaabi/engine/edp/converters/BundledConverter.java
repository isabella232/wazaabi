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

package org.eclipse.wazaabi.engine.edp.converters;

public interface BundledConverter {
	
	public Object convert(Object input);
	public boolean isConverterFor(Class<?> input, Class<?> output);
	public boolean isDisposed();
	public void dispose();
	
}
