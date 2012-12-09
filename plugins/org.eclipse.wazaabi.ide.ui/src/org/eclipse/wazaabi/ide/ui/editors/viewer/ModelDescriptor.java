/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import java.io.Serializable;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.transaction.TransactionalEditingDomain;

public class ModelDescriptor implements Serializable {

	private static final long serialVersionUID = 1L;
	private final String editingDomainId;
	private final String resourceURI;
	private final String uriFragment;

	protected ModelDescriptor(String editingDomainId, String resourceURI,
			String uriFragment) {
		this.editingDomainId = editingDomainId;
		this.resourceURI = resourceURI; // Since emf URI is not serializable
		this.uriFragment = uriFragment;
	}

	public String getEditingDomainId() {
		return editingDomainId;
	}

	public String getResourceURI() {
		return resourceURI;
	}

	public String getUriFragment() {
		return uriFragment;
	}

	public static ModelDescriptor createModelDescriptor(EObject eObject) {
		if (eObject == null || eObject.eResource() == null)
			return null;
		TransactionalEditingDomain editingDomain = TransactionalEditingDomain.Factory.INSTANCE
				.getEditingDomain(eObject.eResource().getResourceSet());
		if (editingDomain == null)
			return null;
		return new ModelDescriptor(editingDomain.getID(), eObject.eResource()
				.getURI().toString(), eObject.eResource().getURIFragment(
				eObject));
	}

	public static EObject getEObject(ModelDescriptor modelDescriptor) {
		if (modelDescriptor == null)
			return null;
		TransactionalEditingDomain editingDomain = TransactionalEditingDomain.Registry.INSTANCE
				.getEditingDomain(modelDescriptor.getEditingDomainId());
		if (editingDomain == null)
			return null;
		Resource resource = editingDomain.getResourceSet().getResource(
				URI.createURI(modelDescriptor.getResourceURI()), false);
		if (resource == null)
			return null;
		return resource.getEObject(modelDescriptor.getUriFragment());
	}
}