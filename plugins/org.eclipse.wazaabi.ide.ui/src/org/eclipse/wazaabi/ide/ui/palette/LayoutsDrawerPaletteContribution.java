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

import org.eclipse.gef.palette.CreationToolEntry;
import org.eclipse.gef.palette.PaletteDrawer;
import org.eclipse.gef.palette.PaletteEntry;
import org.eclipse.wazaabi.ide.ui.PaletteContribution;
import org.eclipse.wazaabi.ide.ui.editors.ContributionBasedPaletteFactory;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class LayoutsDrawerPaletteContribution implements PaletteContribution {

	public static final String LAYOUTS_DRAWER_ID = "layouts"; //$NON-NLS-1$

	/**
	 * Attaches this contribution to the Palette root
	 */
	public String getParentId() {
		return ContributionBasedPaletteFactory.PALETTE_ROOT_ID;
	}

	/**
	 * Takes the third place in the palette
	 */
	public int getDesiredIndex() {
		return 2;
	}

	public PaletteEntry getPaletteEntry() {
		PaletteDrawer drawer = new PaletteDrawer("layouts", null);//$NON-NLS-1$
		drawer.setId(LAYOUTS_DRAWER_ID);

		List<CreationToolEntry> entries = new ArrayList<CreationToolEntry>();

		PaletteFactoryUtils.addCreationToolEntry(entries, "Fill Layout",
				"Create a fill layout",
				SWTStylesPackage.Literals.FILL_LAYOUT_RULE);

		PaletteFactoryUtils.addCreationToolEntry(entries, "Row Layout",
				"Create a row layout",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE);

		PaletteFactoryUtils.addCreationToolEntry(entries, "Row Data",
				"Create a row data", SWTStylesPackage.Literals.ROW_DATA_RULE);

		PaletteFactoryUtils.addCreationToolEntry(entries, "Grid Layout",
				"Create a Grid layout",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE);

		PaletteFactoryUtils.addCreationToolEntry(entries, "Grid Data",
				"Create a Grid data", SWTStylesPackage.Literals.GRID_DATA_RULE);

		PaletteFactoryUtils.addCreationToolEntry(entries, "Stack Layout",
				"Create a stack layout",
				CoreStylesPackage.Literals.STACK_LAYOUT_RULE);

		drawer.addAll(entries);

		ContributionBasedPaletteFactory
				.addPaletteContributionsToContainer(drawer);

		return drawer;
	}

}
