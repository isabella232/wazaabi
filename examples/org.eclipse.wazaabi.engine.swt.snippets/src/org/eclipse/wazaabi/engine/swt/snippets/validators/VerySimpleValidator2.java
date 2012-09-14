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

package org.eclipse.wazaabi.engine.swt.snippets.validators;

import java.util.List;

import org.eclipse.wazaabi.engine.edp.EDP;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class VerySimpleValidator2 {

	public VerySimpleValidator2() {
		System.out.println("creating " + getClass().getName());
	}

	public boolean isValid(TextComponent eventDispatcher,
			EventHandler eventHandler){
		Object result = eventDispatcher.get(EDP.CONVERTER_RESULT_KEY);
		if (result != null) {
			if (result instanceof List)
				if(((List<?>)result).get(0) instanceof String){
					System.out.println("validator 2 executed: conversion result type ok");
					return true;
				}
		}
		eventDispatcher.set("targetError", true);
		System.out.println("validator 2 executed: conversion result type error");
		return false;
			
			
		
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
