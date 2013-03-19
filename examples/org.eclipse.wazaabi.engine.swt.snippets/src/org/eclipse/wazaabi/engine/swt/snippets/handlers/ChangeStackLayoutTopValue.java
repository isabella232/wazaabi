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

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ChangeStackLayoutTopValue {

	public void execute(Widget dispatcher, EventHandler eventHandler,
			Event event) {
		Container container = (Container) dispatcher.eContainer();
		StackLayoutRule stackLayoutRule = (StackLayoutRule) container
				.getFirstStyleRule("layout",
						CoreStylesPackage.Literals.STACK_LAYOUT_RULE);
		stackLayoutRule.setTop(1);
	}
}
