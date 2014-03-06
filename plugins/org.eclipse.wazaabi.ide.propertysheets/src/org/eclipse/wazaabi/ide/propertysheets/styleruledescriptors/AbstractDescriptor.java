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

package org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;

public abstract class AbstractDescriptor {

	private final String id;
	private final String label;
	private final String description;
	private final String packageURI;
	private final String eClassName;
	private AbstractDescriptor container;

	private List<AbstractDescriptor> children = new ArrayList<AbstractDescriptor>();;

	public AbstractDescriptor(String id, String label, String description,
			String packageURI, String eClassName) {
		assert id != null;
		this.id = id;
		this.label = label;
		this.description = description;
		this.packageURI = packageURI;
		this.eClassName = eClassName;
	}

	public abstract EObject createNewInstance();

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AbstractDescriptor other = (AbstractDescriptor) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}

	protected AbstractDescriptor getAncestor(AbstractDescriptor descriptor) {
		if (descriptor.getContainer() == null)
			return descriptor;
		return getAncestor(descriptor.getContainer());
	}

	public List<AbstractDescriptor> getChildren() {
		return children;
	}

	public AbstractDescriptor getContainer() {
		return container;
	}

	public String getDescription() {
		return description;
	}

	public String getEClassName() {
		return eClassName;
	}

	public String getLabel() {
		return label;
	}

	public String getPackageURI() {
		return packageURI;
	}

	public String getId() {
		return id;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		return result;
	}

	public void setContainer(AbstractDescriptor container) {
		this.container = container;
	}

	@Override
	public String toString() {
		return "Descriptor [propertyName=" + id + ", packageURI="
				+ packageURI + ", eClassName=" + eClassName + "]";
	}
}
