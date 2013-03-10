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

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.wazaabi.ide.ui.editors.viewer.AbstractComponentMappingRule;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;

public class OnJDTElementsMappingRules {

	@AbstractComponentMappingRule(targetType = IPackageFragment.class, droppedType = ICompilationUnit.class)
	public List<ICompilationUnit> getButtonOnPackageFragment(
			IPackageFragment target, int index, PushButton source,
			Object context) {

		List<ICompilationUnit> compilationUnits = new ArrayList<ICompilationUnit>();
		try {
			ICompilationUnit compilationUnit = target.createCompilationUnit(
					"hello.java", "Hello World", false,
					new NullProgressMonitor());
			compilationUnits.add(compilationUnit);
		} catch (JavaModelException e) {
			e.printStackTrace();
		}

		return compilationUnits;
	}

}