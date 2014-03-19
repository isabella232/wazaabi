package org.eclipse.e4.tool.emf.ui.wazaabi;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;

public class PDEWazaabiURIContributionProvider implements
		IWazaabiURIContributionProvider {

	public void findContribution(final Filter filter,
			final ContributionResultHandler handler) {

		try {
			filter.project.getWorkspace().getRoot()
					.accept(new IResourceVisitor() {

						@Override
						public boolean visit(IResource resource)
								throws CoreException {
							System.out.println(resource);
							if (resource.getType() == IResource.ROOT
									|| resource.getType() == IResource.PROJECT
									|| resource.getType() == IResource.FOLDER)
								return true;
							if (resource.getType() == IResource.FILE
									&& resource.getName().endsWith(".ui")) {
								IFile file = (IFile) resource;
								handler.result(new ContributionData(file
										.getProject().getName(), file.getName()));
							}
							return false;
						}
					}, IResource.DEPTH_INFINITE, false);
		} catch (CoreException e) {
			e.printStackTrace();
		}

	}

}
