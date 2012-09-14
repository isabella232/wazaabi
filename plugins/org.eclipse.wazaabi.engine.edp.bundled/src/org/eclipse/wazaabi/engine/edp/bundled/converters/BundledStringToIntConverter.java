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

package org.eclipse.wazaabi.engine.edp.bundled.converters;

import org.eclipse.wazaabi.engine.edp.converters.BundledConverter;

public class BundledStringToIntConverter implements BundledConverter {

	boolean isDisposed;
	
	public BundledStringToIntConverter(){
		System.out.println("creating "+this.getClass());
		isDisposed = false;
	}
	
	public Integer convert(Object input) {
		if(input instanceof String)
			return Integer.parseInt((String)input);
		else
			return null;
	}

	public boolean isConverterFor(Class<?> input, Class<?> output) {
		if(input == String.class)
			if(output == Integer.class)
				return true;
		return false;
	}

	
	public void dispose() {
		this.isDisposed =  true;
		// TODO Auto-generated method stub
		
	}

	
	public boolean isDisposed() {
		return isDisposed;
	}

}
