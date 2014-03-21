/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
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
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;

public class OnCollectionMappingRules {

	@EAttributeMappingRule(datatype = "EEnum")
	public List<Binding> getEEnumOnCollectionBindings(Collection target,
			int index, EAttribute source, Object context) {
		List<Binding> bindings = new ArrayList<Binding>();

		Binding model2UIBinding = MappingUtils.createBinding("$input/@"
				+ source.getName(), "@selection");
		MappingUtils.addPropertyChangedEvent(model2UIBinding, "$input/@"
				+ source.getName());
		MappingUtils.addEvent(model2UIBinding, "core:ui:refresh");
		bindings.add(model2UIBinding);

		Binding UI2ModelBinding = MappingUtils.createBinding("@selection",
				"$input/@" + source.getName());
		MappingUtils.addPropertyChangedEvent(UI2ModelBinding, "@selection");
		bindings.add(UI2ModelBinding);
		return bindings;
	}

}
