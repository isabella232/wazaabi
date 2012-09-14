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

package org.eclipse.wazaabi.engine.swt.demo;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.core.widgets.Widget;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class ToggleDemoAction {

	public ToggleDemoAction() {
		System.out.println("creating " + getClass().getName());
	}

	public void execute(Widget dispatcher,
			EventHandler eventHandler, Event event) {
		
		Container mainContainer = (Container) ((PushButton) dispatcher).eContainer();
		Container rootContainer = (Container) mainContainer.eContainer();
		Container tabbedContainer = (Container) rootContainer.getChildren().get(1);
		
		for (StyleRule styleRule:tabbedContainer.getStyleRules()){
			if (styleRule instanceof TabbedLayoutRule){
				ExpandLayoutRule expandLayout = CoreStylesFactory.eINSTANCE.createExpandLayoutRule();
				expandLayout.setPropertyName("layout");
				
				tabbedContainer.getStyleRules().remove(styleRule);
				tabbedContainer.getStyleRules().add(expandLayout);
				
				break;
			}
			if (styleRule instanceof ExpandLayoutRule) {
				TabbedLayoutRule tabbedLayout = CoreStylesFactory.eINSTANCE.createTabbedLayoutRule();
				tabbedLayout.setPropertyName("layout");
				tabbedLayout.setTop(1);
				
				tabbedContainer.getStyleRules().remove(styleRule);
				tabbedContainer.getStyleRules().add(tabbedLayout);
				
				break;
			}
		}
		
		System.out.println("style toggled");
		
	}

	public void dispose() {
		System.out.println("disposing " + getClass().getName());
	}
}
