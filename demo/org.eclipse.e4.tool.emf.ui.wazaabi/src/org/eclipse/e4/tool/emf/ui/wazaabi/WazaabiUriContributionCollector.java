package org.eclipse.e4.tool.emf.ui.wazaabi;

import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.e4.tool.emf.ui.wazaabi.IWazaabiURIContributionProvider.ContributionResultHandler;

public class WazaabiUriContributionCollector {
	private CopyOnWriteArrayList<IWazaabiURIContributionProvider> providers = new CopyOnWriteArrayList<IWazaabiURIContributionProvider>();

	public void addContributor(IWazaabiURIContributionProvider contributor) {
		providers.add(contributor);
	}

	public void removeContributor(IWazaabiURIContributionProvider contributor) {
		providers.remove(contributor);
	}

	public void findContributions(
			IWazaabiURIContributionProvider.Filter filter,
			ContributionResultHandler resultHandler) {

		for (IWazaabiURIContributionProvider contributor : providers) {
			contributor.findContribution(filter, resultHandler);
		}
	}

}
