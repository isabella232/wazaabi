package org.eclipse.wazaabi.ide.ui.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.ide.ui.PaletteContribution;

public class PaletteContributionRegistry {

	private List<PaletteContribution> paletteContributions = new ArrayList<PaletteContribution>();

	public void addPaletteContribution(PaletteContribution paletteContribution) {
		System.out.println("adding: " + paletteContribution);
		getPaletteContributions().add(paletteContribution);
	}

	public void removePaletteContribution(
			PaletteContribution paletteContribution) {
		getPaletteContributions().remove(paletteContribution);
		System.out.println("removed: " + paletteContribution);
	}

	public List<PaletteContribution> getPaletteContributions() {
		return paletteContributions;
	}

}
