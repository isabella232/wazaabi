/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.gef.palette.CreationToolEntry;
import org.eclipse.gef.palette.PaletteContainer;
import org.eclipse.gef.palette.PaletteDrawer;
import org.eclipse.gef.palette.PaletteGroup;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.gef.palette.PanningSelectionToolEntry;
import org.eclipse.gef.palette.ToolEntry;
import org.eclipse.wazaabi.ide.ui.PaletteContribution;
import org.eclipse.wazaabi.ide.ui.internal.Activator;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class PaletteFactory {

	public static final String COMPONENTS_DRAWER_ID = "components"; //$NON-NLS-1$
	public static final String LAYOUTS_DRAWER_ID = "layouts"; //$NON-NLS-1$

	public void createChildren(PaletteRoot paletteRoot) {
		paletteRoot.add(createControlGroup(paletteRoot));
		paletteRoot.add(createComponentsDrawer());
		paletteRoot.add(createLayoutsDrawer());
	}

	private PaletteContainer createControlGroup(PaletteRoot root) {
		PaletteGroup controlGroup = new PaletteGroup("ooio");

		List<ToolEntry> entries = new ArrayList<ToolEntry>();

		ToolEntry tool = new PanningSelectionToolEntry();
		entries.add(tool);
		root.setDefaultEntry(tool);
		controlGroup.addAll(entries);
		return controlGroup;
	}

	private PaletteContainer createComponentsDrawer() {
		PaletteDrawer drawer = new PaletteDrawer("components", null);//$NON-NLS-1$
		drawer.setId(COMPONENTS_DRAWER_ID);
		List<CreationToolEntry> entries = new ArrayList<CreationToolEntry>();

		addComponentEntry(entries, "Container", "Create a container",
				CoreWidgetsPackage.Literals.CONTAINER);
		addComponentEntry(entries, "PushButton", "Create a push button",
				CoreWidgetsPackage.Literals.PUSH_BUTTON);
		addComponentEntry(entries, "TextComponent", "Create a text component",
				CoreWidgetsPackage.Literals.TEXT_COMPONENT);
		addComponentEntry(entries, "RadioButton", "Create a Radio button",
				CoreWidgetsPackage.Literals.RADIO_BUTTON);
		addComponentEntry(entries, "CheckBox", "Create a CheckBox",
				CoreWidgetsPackage.Literals.CHECK_BOX);
		addComponentEntry(entries, "Label", "Create a Label",
				CoreWidgetsPackage.Literals.LABEL);
		addComponentEntry(entries, "Scale", "Create a Scale",
				CoreWidgetsPackage.Literals.SCALE);
		addComponentEntry(entries, "Slider", "Create a Slider",
				CoreWidgetsPackage.Literals.SLIDER);
		addComponentEntry(entries, "Spinner", "Create a Spinner",
				CoreWidgetsPackage.Literals.SPINNER);

		drawer.addAll(entries);
		addPaletteContributions(drawer);
		return drawer;
	}

	private PaletteContainer createLayoutsDrawer() {
		PaletteDrawer drawer = new PaletteDrawer("layouts", null);//$NON-NLS-1$
		drawer.setId(LAYOUTS_DRAWER_ID);

		List<CreationToolEntry> entries = new ArrayList<CreationToolEntry>();

		addComponentEntry(entries, "Fill Layout", "Create a fill layout",
				SWTStylesPackage.Literals.FILL_LAYOUT_RULE);

		addComponentEntry(entries, "Row Layout", "Create a row layout",
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE);

		addComponentEntry(entries, "Row Data", "Create a row data",
				SWTStylesPackage.Literals.ROW_DATA_RULE);

		addComponentEntry(entries, "Grid Layout", "Create a Grid layout",
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE);

		addComponentEntry(entries, "Grid Data", "Create a Grid data",
				SWTStylesPackage.Literals.GRID_DATA_RULE);

		addComponentEntry(entries, "Stack Layout", "Create a stack layout",
				CoreStylesPackage.Literals.STACK_LAYOUT_RULE);

		drawer.addAll(entries);
		addPaletteContributions(drawer);

		return drawer;
	}

	protected void addComponentEntry(List<CreationToolEntry> entries,
			String label, String shortDesc, EClass eClass) {
		CreationToolEntry entry = new CreationToolEntry(label, shortDesc,
				new EMFPaletteCreationFactory(eClass), Activator.getDefault()
						.getImageRegistry().getDescriptor(eClass.getName()),
				null);
		entries.add(entry);
	}

	protected void addPaletteContributions(PaletteDrawer drawer) {
		if (drawer == null || drawer.getId() == null
				|| "".equals(drawer.getId())) //$NON-NLS-1$
			return;
		for (PaletteContribution paletteContribution : Activator
				.getPaletteContributionRegistry().getPaletteContributions()) {
			if (drawer.getId().equals(paletteContribution.getParentId())
					&& paletteContribution.getPaletteEntry() instanceof CreationToolEntry) {
				drawer.add(((CreationToolEntry) paletteContribution
						.getPaletteEntry()));
			}
		}
	}
}
