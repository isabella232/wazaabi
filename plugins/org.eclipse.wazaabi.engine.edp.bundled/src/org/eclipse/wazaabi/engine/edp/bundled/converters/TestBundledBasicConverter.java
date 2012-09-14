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


public class TestBundledBasicConverter implements BundledConverter {

	boolean disposed = true;

	public TestBundledBasicConverter() {
		System.out.println("creating " + getClass().getName());
		disposed = false;
	}

	public Integer convert(Object a) {
		System.out.println("pushButton pressed");
		//System.out.println(dispatcher.getText());
		//dispatcher.setText("New");
		return ((Integer)a) + 6;
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
		disposed = true;
	}
	
	public boolean isDisposed() {
		return disposed;
	}

	public boolean isConverterFor(Class<?> input, Class<?> output) {
		// TODO Auto-generated method stub
		return false;
	}


}
