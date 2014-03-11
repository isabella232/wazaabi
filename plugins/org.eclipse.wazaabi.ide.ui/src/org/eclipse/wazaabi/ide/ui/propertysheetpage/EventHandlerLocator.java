/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.propertysheetpage;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;
import org.eclipse.osgi.util.ManifestElement;
import org.eclipse.wazaabi.ide.propertysheets.MethodLocator;
import org.osgi.framework.BundleException;
import org.osgi.framework.Constants;

public class EventHandlerLocator extends MethodLocator {

	public List<String> getURIs(String methodName, final int argsCount) {
		final List<String> result = new ArrayList<String>();
		for (IProject project : ResourcesPlugin.getWorkspace().getRoot()
				.getProjects()) {
			try {
				if (!project.isNatureEnabled("org.eclipse.jdt.core.javanature"))
					continue;
			} catch (CoreException e1) {
				e1.printStackTrace();
			}
			IJavaProject javaProject = JavaCore.create(project);

			List<IPackageFragment> packages = new ArrayList<IPackageFragment>();
			try {

				for (IPackageFragment p : javaProject.getPackageFragments())
					// Package fragments include all packages in the classpath
					// We will only look at the package from the source folder
					// K_BINARY would include also included JARS, e.g. rt.jar
					if (p.getKind() == IPackageFragmentRoot.K_SOURCE)
						packages.add(p);
			} catch (JavaModelException e) {
				e.printStackTrace();
			}

			SearchPattern pattern = SearchPattern.createPattern(methodName,
					IJavaSearchConstants.METHOD,
					IJavaSearchConstants.DECLARATIONS,
					SearchPattern.R_EXACT_MATCH);

			IJavaSearchScope scope = SearchEngine
					.createJavaSearchScope(packages
							.toArray(new IPackageFragment[] {}));

			SearchRequestor requestor = new SearchRequestor() {
				public void acceptSearchMatch(SearchMatch match) {
					try {
						if (match.getElement() instanceof IMethod
								&& ((IMethod) match.getElement())
										.getParameters().length == argsCount) {
							String uri = getURI((IMethod) match.getElement());
							if (uri != null)
								result.add(uri);
						}
					} catch (JavaModelException e) {
						e.printStackTrace();
					}
				}
			};

			SearchEngine searchEngine = new SearchEngine();
			try {
				searchEngine.search(pattern,
						new SearchParticipant[] { SearchEngine
								.getDefaultSearchParticipant() }, scope,
						requestor, null /* progress monitor */);
			} catch (CoreException e) {
				System.out.println("exception");
				e.printStackTrace();
			}

		}

		return result;
	}

	protected String getBundleSymbolicName(IJavaProject javaProject) {
		IFile manifestFile = javaProject.getProject().getFile(
				new Path("META-INF/MANIFEST.MF")); //$NON-NLS-1$
		if (manifestFile != null && manifestFile.exists()) {
			HashMap<String, String> manifestValues = new HashMap<String, String>();
			try {
				InputStream manifestContent = manifestFile.getContents();
				ManifestElement.parseBundleManifest(manifestContent,
						manifestValues);

				ManifestElement[] symbolicNameElements = ManifestElement
						.parseHeader(Constants.BUNDLE_SYMBOLICNAME,
								manifestValues
										.get(Constants.BUNDLE_SYMBOLICNAME));
				if (symbolicNameElements.length > 0
						&& symbolicNameElements[0].getValueComponents().length > 0)
					return symbolicNameElements[0].getValueComponents()[0];
			} catch (CoreException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (BundleException e) {
				e.printStackTrace();
			}
		}
		return null;
	}

	protected String getURI(IMethod method) {
		ICompilationUnit cu = (ICompilationUnit) method.getTypeRoot();
		String className = cu.getElementName().substring(0,
				cu.getElementName().length() - 5);
		String packageName = ((IPackageFragment) cu.getParent())
				.getElementName();
		String bundleSymbolicName = getBundleSymbolicName(method
				.getJavaProject());
		if (bundleSymbolicName != null && !bundleSymbolicName.isEmpty())
			return "platform://plugin/" + bundleSymbolicName + "/"
					+ packageName + "." + className;
		return null;
	}

}
