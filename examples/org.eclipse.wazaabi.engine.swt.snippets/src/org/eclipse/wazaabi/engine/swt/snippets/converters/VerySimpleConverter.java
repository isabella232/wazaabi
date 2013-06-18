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

package org.eclipse.wazaabi.engine.swt.snippets.converters;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.engine.edp.EDP;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class VerySimpleConverter {

	public VerySimpleConverter() {
		System.out.println("creating " + getClass().getName());
	}

	public void execute(Widget dispatcher, EventHandler eventHandler,
			Event event) {
		System.out.println("converter executed");
		if (dispatcher.get(EDP.VALUE_SOURCE_KEY) instanceof List<?>
				&& ((List<?>) dispatcher.get(EDP.VALUE_SOURCE_KEY)).get(0) instanceof String) {
			String oldText = (String) (((List<?>) dispatcher
					.get(EDP.VALUE_SOURCE_KEY)).get(0));
			List<String> result = new ArrayList<String>();
			result.add(oldText + " converted");
			dispatcher.set(EDP.CONVERTER_RESULT_KEY, result);
		} else
			System.out.println("nothing to convert");

	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
