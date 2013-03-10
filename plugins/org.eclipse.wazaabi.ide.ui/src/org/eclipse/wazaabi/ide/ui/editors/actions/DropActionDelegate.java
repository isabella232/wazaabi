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

package org.eclipse.wazaabi.ide.ui.editors.actions;

import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.ui.part.IDropActionDelegate;
import org.eclipse.wazaabi.ide.mapping.rules.MappingUtils;
import org.eclipse.wazaabi.ide.mapping.sourcecode.CompilationUnitDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ModelDescriptorTransfert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DropActionDelegate implements IDropActionDelegate {
	final static Logger logger = LoggerFactory
			.getLogger(DropActionDelegate.class);

	public boolean run(Object source, Object target) {

		if (target instanceof IPackageFragment) {
			IPackageFragment pf = (IPackageFragment) target;

			ModelDescriptor modelDescriptors[] = ModelDescriptorTransfert
					.getInstance().fromByteArray((byte[]) source);
			if (modelDescriptors != null)
				for (ModelDescriptor modelDescriptor : modelDescriptors) {
					TransactionalEditingDomain t = TransactionalEditingDomain.Registry.INSTANCE
							.getEditingDomain(modelDescriptor
									.getEditingDomainId());
					if (t != null) {
						Resource r = t.getResourceSet()
								.getResource(
										URI.createURI(modelDescriptor
												.getResourceURI()), false);
						if (r != null) {
							EObject realSource = r.getEObject(modelDescriptor
									.getUriFragment());
							@SuppressWarnings({ "unchecked" })
							List<CompilationUnitDescriptor> compilationUnits = (List<CompilationUnitDescriptor>) MappingUtils
									.getFFactory().get(target,
											IPackageFragment.class, 0,
											realSource,
											CompilationUnitDescriptor.class,
											null);
							System.out.println(compilationUnits);
						}
					}
				}

			// try {
			// pf.createCompilationUnit("toto.java", "ooo", true,
			// new NullProgressMonitor());
			// } catch (JavaModelException e) {
			// e.printStackTrace();
			// }
		}

		if (target instanceof IContainer) {
			ModelDescriptor modelDescriptors[] = ModelDescriptorTransfert
					.getInstance().fromByteArray((byte[]) source);

			// IContainer parent = (IContainer) target;
			for (ModelDescriptor modelDescriptor : modelDescriptors)
				logger.debug("received ModelDescriptor: {}", modelDescriptor);
			return true;
		}
		return false;
	}
}