package org.eclipse.wazaabi.engine.edp.adapters;

import java.util.List;

import org.eclipse.wazaabi.engine.edp.coderesolution.ExecutableAdapter;

public interface SequenceAdapter extends ExecutableAdapter {

	public List<ExecutableAdapter> getExecutableAdapters();

}
