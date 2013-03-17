/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors.viewer.bindingrules;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.wazaabi.ide.mapping.annotations.EAttributeMappingRule;
import org.eclipse.wazaabi.ide.mapping.rules.MappingUtils;
import org.eclipse.wazaabi.mm.core.widgets.TextComponent;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class OnTextComponentMapping {

	@EAttributeMappingRule(datatype = "EString")
	public List<EventHandler> getEStringOnTextComponentEventHandlers(
			TextComponent target, int index, EAttribute source, Object context) {
		List<EventHandler> eventHandlers = new ArrayList<EventHandler>();
		// Binding binding = EDPHandlersFactory.eINSTANCE.createBinding();
		// eventHandlers.add(binding);
		return eventHandlers;
	}

	@EAttributeMappingRule(datatype = "EString")
	public List<Binding> getEStringOnTextComponentBindings(
			TextComponent target, int index, EAttribute source, Object context) {
		List<Binding> bindings = new ArrayList<Binding>();
		Binding model2UIBinding = MappingUtils.createBinding("$input/@"
				+ source.getName(), "@text");
		MappingUtils.addPropertyChangedEvent(model2UIBinding, "$input/@"
				+ source.getName());
		bindings.add(model2UIBinding);
		Binding UI2ModelBinding = MappingUtils.createBinding("@text",
				"$input/@" + source.getName());
		MappingUtils.addEvent(UI2ModelBinding, "core:ui:focus:out");
		bindings.add(UI2ModelBinding);
		return bindings;
	}

}