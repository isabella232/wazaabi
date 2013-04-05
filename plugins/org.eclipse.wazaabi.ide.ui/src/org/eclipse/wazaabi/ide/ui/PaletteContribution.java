package org.eclipse.wazaabi.ide.ui;

import org.eclipse.gef.palette.PaletteEntry;

public interface PaletteContribution {

	public String getParentId();

	public int getDesiredIndex();

	public PaletteEntry getPaletteEntry();

}
