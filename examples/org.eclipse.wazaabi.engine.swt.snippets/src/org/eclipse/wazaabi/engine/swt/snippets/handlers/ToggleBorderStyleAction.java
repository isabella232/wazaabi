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

package org.eclipse.wazaabi.engine.swt.snippets.handlers;

import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ToggleBorderStyleAction {

	public ToggleBorderStyleAction() {
		System.out.println("creating " + getClass().getName());
	}

	public void execute(Widget dispatcher,
			EventHandler eventHandler, Event event) {
		int i=0;
		for (StyleRule styleRule:dispatcher.getStyleRules()){
			if (styleRule instanceof BooleanRule){
				((BooleanRule) styleRule).setValue(!((BooleanRule)(styleRule)).isValue());
				break;
			}
		}
		
		System.out.println("border toggled");
		
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
