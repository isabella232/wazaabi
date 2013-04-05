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

package org.eclipse.wazaabi.ide.ui.editors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.gef.palette.PaletteContainer;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.wazaabi.ide.ui.PaletteContribution;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public class ContributionBasedPaletteFactory {

	public static final String PALETTE_ROOT_ID = "root"; //$NON-NLS-1$

	public void createChildren(PaletteRoot paletteRoot) {
		addPaletteContributionsToContainer(paletteRoot);
	}

	public static void addPaletteContributionsToContainer(
			PaletteContainer container,
			List<PaletteContribution> paletteContributions) {
		if (paletteContributions != null)
			addPaletteContributionsToContainer(container,
					paletteContributions.toArray(new PaletteContribution[] {}));
	}

	public static void addPaletteContributionsToContainer(
			PaletteContainer container,
			PaletteContribution paletteContributions[]) {
		if (container == null || container.getId() == null
				|| "".equals(container.getId())) //$NON-NLS-1$
			return;
		if (paletteContributions == null)
			return;
		int maxIndex = -1;
		HashMap<Integer, List<PaletteContribution>> paletteContributionsSortedByIndex = new HashMap<Integer, List<PaletteContribution>>();
		for (PaletteContribution paletteContribution : paletteContributions)
			if (container.getId().equals(paletteContribution.getParentId())) {
				List<PaletteContribution> sameIndexPaletteContributions = paletteContributionsSortedByIndex
						.get(new Integer(paletteContribution.getDesiredIndex()));
				if (sameIndexPaletteContributions == null) {
					sameIndexPaletteContributions = new ArrayList<PaletteContribution>();
					paletteContributionsSortedByIndex.put(new Integer(
							paletteContribution.getDesiredIndex()),
							sameIndexPaletteContributions);
				}
				sameIndexPaletteContributions.add(paletteContribution);
				maxIndex = Math.max(maxIndex,
						paletteContribution.getDesiredIndex());
			}

		for (int i = 0; i <= maxIndex; i++) {
			List<PaletteContribution> sameIndexPaletteContributions = paletteContributionsSortedByIndex
					.get(new Integer(i));
			if (sameIndexPaletteContributions != null)
				for (PaletteContribution paletteContribution : sameIndexPaletteContributions)
					container.add(paletteContribution.getPaletteEntry());

		}
	}

	public static void addPaletteContributionsToContainer(
			PaletteContainer container) {

		addPaletteContributionsToContainer(container, Activator
				.getPaletteContributionRegistry().getPaletteContributions());
	}

}
