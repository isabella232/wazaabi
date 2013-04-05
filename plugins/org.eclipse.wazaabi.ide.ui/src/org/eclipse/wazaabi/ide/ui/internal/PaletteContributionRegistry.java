package org.eclipse.wazaabi.ide.ui.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wazaabi.ide.ui.PaletteContribution;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PaletteContributionRegistry {

	final static Logger logger = LoggerFactory
			.getLogger(PaletteContributionRegistry.class);

	private List<PaletteContribution> paletteContributions = new ArrayList<PaletteContribution>();

	public void addPaletteContribution(PaletteContribution paletteContribution) {
		logger.debug("adding: {}", paletteContribution);
		getPaletteContributions().add(paletteContribution);
	}

	public void removePaletteContribution(
			PaletteContribution paletteContribution) {
		getPaletteContributions().remove(paletteContribution);
		logger.debug("removed: {}", paletteContribution);
	}

	public List<PaletteContribution> getPaletteContributions() {
		return paletteContributions;
	}

}
