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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl;

public class StyleRuleDescriptor extends AbstractDescriptor {

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

	public StyleRuleDescriptor(String propertyName, String label,
			String description, String packageURI, String eClassName) {
		super(propertyName, label, description, packageURI, eClassName);
	}

	@Override
	public EObject createNewInstance() {
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
			styleRule.setPropertyName(getAncestor(this).getId());
			return styleRule;
		}
		return null;
	}

	public String getPropertyName() {
		return getId();
	}

}
