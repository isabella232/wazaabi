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
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.wazaabi.ide.mapping.annotations.AbstractComponentMappingRule;
import org.eclipse.wazaabi.ide.mapping.sourcecode.CompilationUnitDescriptor;
import org.eclipse.wazaabi.mm.core.widgets.PushButton;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;

public class OnJDTElementsMappingRules {

	@AbstractComponentMappingRule
	public List<CompilationUnitDescriptor> getButtonOnPackageFragment(
			IPackageFragment target, int index, PushButton source,
			Object context) {

		if (target == null || !target.exists())
			return Collections.emptyList();

		List<CompilationUnitDescriptor> compilationUnits = new ArrayList<CompilationUnitDescriptor>();

		String sourceName = source.eClass().getName();
		String className = sourceName + "EventHandler";
		String sourceFileName = className + ".java";

		int idx = 0;
		while (target.getCompilationUnit(sourceFileName).exists()) {
			sourceFileName = className + (++idx) + ".java";
		}
		if (idx != 0)
			className += idx;
		StringBuffer buffer = new StringBuffer();
		buffer.append("package " + target.getElementName() + ";\n");
		buffer.append("\n");
		buffer.append("import ");
		buffer.append(source.eClass().getInstanceClassName());
		buffer.append(";\n");
		buffer.append("import ");
		buffer.append(EventHandler.class.getName());
		buffer.append(";\n");
		buffer.append("import ");
		buffer.append(Event.class.getName());
		buffer.append(";\n");
		buffer.append("\n");
		buffer.append("public class ");
		buffer.append(className);
		buffer.append(" {\n\n");
		buffer.append("	public void execute(");
		buffer.append(sourceName);
		buffer.append(" ");
		buffer.append(Character.toLowerCase(sourceName.charAt(0)));
		buffer.append(sourceName.substring(1));
		buffer.append(", EventHandler eventHandler, Event event) {");
		buffer.append("}");
		buffer.append("}");

		CompilationUnitDescriptor compilationUnit = new CompilationUnitDescriptor(
				sourceFileName, buffer.toString());
		compilationUnits.add(compilationUnit);

		return compilationUnits;
	}
}