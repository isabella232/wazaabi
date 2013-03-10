/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
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

import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.wazaabi.ide.mapping.annotations.AbstractComponentMappingRule;
import org.eclipse.wazaabi.ide.mapping.sourcecode.CompilationUnitDescriptor;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;

public class OnJDTElementsMappingRules {

	@AbstractComponentMappingRule
	public List<CompilationUnitDescriptor> getButtonOnPackageFragment(
			IPackageFragment target, int index, PushButton source,
			Object context) {

		List<CompilationUnitDescriptor> compilationUnits = new ArrayList<CompilationUnitDescriptor>();
		CompilationUnitDescriptor compilationUnit = null;
		compilationUnits.add(compilationUnit);

		return compilationUnits;
	}

}