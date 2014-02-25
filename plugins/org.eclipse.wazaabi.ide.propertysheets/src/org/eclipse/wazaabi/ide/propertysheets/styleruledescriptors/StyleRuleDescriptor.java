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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EFactory;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl;

public class StyleRuleDescriptor {

	public static class PlaceHolderRule extends BlankRuleImpl {
		private final StyleRuleDescriptor styleRuleDescriptor;

		public PlaceHolderRule(StyleRuleDescriptor styleRuleDescriptor) {
			this.styleRuleDescriptor = styleRuleDescriptor;
		}

		@Override
		public String getPropertyName() {
			return styleRuleDescriptor.getPropertyName();
		}

		public StyleRuleDescriptor getStyleRuleDescriptor() {
			return styleRuleDescriptor;
		}

	}
	private final String propertyName;
	private final String label;
	private final String description;
	private final String packageURI;
	private final String eClassName;
	private StyleRuleDescriptor container;

	private List<StyleRuleDescriptor> children = new ArrayList<StyleRuleDescriptor>();;

	public StyleRuleDescriptor(String propertyName, String label,
			String description, String packageURI, String eClassName) {
		assert propertyName != null;
		this.propertyName = propertyName;
		this.label = label;
		this.description = description;
		this.packageURI = packageURI;
		this.eClassName = eClassName;
	}

	public StyleRule createNewStyleRule() {
		EFactory factory = EPackage.Registry.INSTANCE
				.getEFactory(getPackageURI());
		EPackage ePackage = EPackage.Registry.INSTANCE
				.getEPackage(getPackageURI());
		EClass eClass = null;
		if (ePackage != null)
			eClass = (EClass) ePackage.getEClassifier(getEClassName());
		if (eClass.isInterface() || eClass.isAbstract())
			return new PlaceHolderRule(this);
		if (factory != null && eClass != null) {
			StyleRule styleRule = (StyleRule) factory.create(eClass);
			styleRule.setPropertyName(getAncestor(this).getPropertyName());
			return styleRule;
		}
		return null;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		StyleRuleDescriptor other = (StyleRuleDescriptor) obj;
		if (propertyName == null) {
			if (other.propertyName != null)
				return false;
		} else if (!propertyName.equals(other.propertyName))
			return false;
		return true;
	}

	protected StyleRuleDescriptor getAncestor(StyleRuleDescriptor descriptor) {
		if (descriptor.getContainer() == null)
			return descriptor;
		return getAncestor(descriptor.getContainer());
	}

	public List<StyleRuleDescriptor> getChildren() {
		return children;
	}

	public StyleRuleDescriptor getContainer() {
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

	public String getPropertyName() {
		return propertyName;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((propertyName == null) ? 0 : propertyName.hashCode());
		return result;
	}

	public void setContainer(StyleRuleDescriptor container) {
		this.container = container;
	}

	@Override
	public String toString() {
		return "StyleRuleDescriptor [propertyName=" + propertyName
				+ ", packageURI=" + packageURI + ", eClassName=" + eClassName
				+ "]";
	}
}
