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

package org.eclipse.wazaabi.ide.ui.palette;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.gef.palette.PaletteEntry;
import org.eclipse.gef.palette.PaletteGroup;
import org.eclipse.gef.palette.PanningSelectionToolEntry;
import org.eclipse.gef.palette.ToolEntry;
import org.eclipse.wazaabi.ide.ui.PaletteContribution;
import org.eclipse.wazaabi.ide.ui.editors.ContributionBasedPaletteFactory;

public class ControlGroupPaletteContribution implements PaletteContribution {

	/**
	 * Attaches this contribution to the Palette root
	 */
	public String getParentId() {
		return ContributionBasedPaletteFactory.PALETTE_ROOT_ID;
	}

	/**
	 * Takes the first place in the palette
	 */
	public int getDesiredIndex() {
		return 0;
	}

	public PaletteEntry getPaletteEntry() {
		PaletteGroup controlGroup = new PaletteGroup("controlGroup"); //$NON-NLS-1$

		List<ToolEntry> entries = new ArrayList<ToolEntry>();

		ToolEntry tool = new PanningSelectionToolEntry();
		entries.add(tool);
		// root.setDefaultEntry(tool);
		controlGroup.addAll(entries);
		return controlGroup;
	}

}
