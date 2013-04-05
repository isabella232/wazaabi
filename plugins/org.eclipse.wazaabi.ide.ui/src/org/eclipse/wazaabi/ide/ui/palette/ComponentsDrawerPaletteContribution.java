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
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

public class ComponentsDrawerPaletteContribution implements PaletteContribution {

	public static final String COMPONENTS_DRAWER_ID = "components"; //$NON-NLS-1$

	/**
	 * Attaches this contribution to the Palette root
	 */
	public String getParentId() {
		return ContributionBasedPaletteFactory.PALETTE_ROOT_ID;
	}

	/**
	 * Takes the second place in the palette
	 */
	public int getDesiredIndex() {
		return 1;
	}

	public PaletteEntry getPaletteEntry() {
		PaletteDrawer drawer = new PaletteDrawer("components", null);//$NON-NLS-1$
		drawer.setId(COMPONENTS_DRAWER_ID);

		List<CreationToolEntry> entries = new ArrayList<CreationToolEntry>();

		PaletteFactoryUtils.addCreationToolEntry(entries, "Container",
				"Create a container", CoreWidgetsPackage.Literals.CONTAINER);
		PaletteFactoryUtils
				.addCreationToolEntry(entries, "PushButton",
						"Create a push button",
						CoreWidgetsPackage.Literals.PUSH_BUTTON);
		PaletteFactoryUtils.addCreationToolEntry(entries, "TextComponent",
				"Create a text component",
				CoreWidgetsPackage.Literals.TEXT_COMPONENT);
		PaletteFactoryUtils.addCreationToolEntry(entries, "RadioButton",
				"Create a Radio button",
				CoreWidgetsPackage.Literals.RADIO_BUTTON);
		PaletteFactoryUtils.addCreationToolEntry(entries, "CheckBox",
				"Create a CheckBox", CoreWidgetsPackage.Literals.CHECK_BOX);
		PaletteFactoryUtils.addCreationToolEntry(entries, "Label",
				"Create a Label", CoreWidgetsPackage.Literals.LABEL);
		PaletteFactoryUtils.addCreationToolEntry(entries, "Scale",
				"Create a Scale", CoreWidgetsPackage.Literals.SCALE);
		PaletteFactoryUtils.addCreationToolEntry(entries, "Slider",
				"Create a Slider", CoreWidgetsPackage.Literals.SLIDER);
		PaletteFactoryUtils.addCreationToolEntry(entries, "Spinner",
				"Create a Spinner", CoreWidgetsPackage.Literals.SPINNER);

		drawer.addAll(entries);

		ContributionBasedPaletteFactory
				.addPaletteContributionsToContainer(drawer);

		return drawer;

	}

}
