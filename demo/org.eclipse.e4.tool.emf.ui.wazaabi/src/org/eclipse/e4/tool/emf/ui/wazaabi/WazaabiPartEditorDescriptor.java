package org.eclipse.e4.tool.emf.ui.wazaabi;

import org.eclipse.e4.tools.emf.ui.common.IEditorDescriptor;
import org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.WazaabiE4Package;
import org.eclipse.emf.ecore.EClass;

public class WazaabiPartEditorDescriptor implements IEditorDescriptor {

	@Override
	public EClass getEClass() {
		return WazaabiE4Package.Literals.WAZAABI_PART;
	}

	@Override
	public Class<?> getEditorClass() {
		return WazaabiPartEditor.class;
	}

}
