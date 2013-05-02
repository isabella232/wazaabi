/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.locationpaths.tests;

import java.io.IOException;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;

public abstract class AbstractTest {

	private static EPackage rootEPackage = null;

	public static final String ROOT_EPACKAGE_NAME = "rootEPackage"; //$NON-NLS-1$
	public static final String ROOT_EPACKAGE_PREFIX = "rootP"; //$NON-NLS-1$
	public static final String ROOT_EPACKAGE_NSURI = "urn:root-epackage"; //$NON-NLS-1$

	public static final String SUB_EPACKAGE1_NAME = "subEPackage1"; //$NON-NLS-1$
	public static final String SUB_EPACKAGE1_PREFIX = "sub1"; //$NON-NLS-1$
	public static final String SUB_EPACKAGE1_NSURI = "urn:sub-epackage"; //$NON-NLS-1$

	public static final String ECLASS1_NAME = "eClass1"; //$NON-NLS-1$
	public static final String ECLASS2_NAME = "eClass2"; //$NON-NLS-1$
	public static final String SUB_ECLASS1_NAME = "SubEClass1"; //$NON-NLS-1$
	public static final String SUB_ECLASS1_ATTR1_NAME = "SubEClassAttr1"; //$NON-NLS-1$
	public static final String SUB_ECLASS1_ATTR2_NAME = "SubEClassAttr1"; //$NON-NLS-1$

	/**
	 * This method creates a model using EMF reflection. The root of this model
	 * is an EPackage.
	 * 
	 * @return
	 */
	protected static EPackage getTestEPackage() {
		if (rootEPackage == null) {
			rootEPackage = EcoreFactory.eINSTANCE.createEPackage();
			rootEPackage.setName(ROOT_EPACKAGE_NAME);
			rootEPackage.setNsURI(ROOT_EPACKAGE_PREFIX);
			rootEPackage.setNsURI(ROOT_EPACKAGE_NSURI);

			EClass eClass1 = EcoreFactory.eINSTANCE.createEClass();
			eClass1.setName(ECLASS1_NAME);
			rootEPackage.getEClassifiers().add(eClass1);

			EPackage subEPackage1 = EcoreFactory.eINSTANCE.createEPackage();
			subEPackage1.setName(SUB_EPACKAGE1_NAME);
			subEPackage1.setNsURI(SUB_EPACKAGE1_PREFIX);
			subEPackage1.setNsURI(SUB_EPACKAGE1_NSURI);
			rootEPackage.getESubpackages().add(subEPackage1);

			EClass subEClass1 = EcoreFactory.eINSTANCE.createEClass();
			subEClass1.setName(SUB_ECLASS1_NAME);
			subEPackage1.getEClassifiers().add(subEClass1);

			EAttribute attr1 = EcoreFactory.eINSTANCE.createEAttribute();
			attr1.setName(SUB_ECLASS1_ATTR1_NAME);
			attr1.setEType(EcorePackage.Literals.ESTRING);
			subEClass1.getEStructuralFeatures().add(attr1);

			EAttribute attr2 = EcoreFactory.eINSTANCE.createEAttribute();
			attr2.setName(SUB_ECLASS1_ATTR2_NAME);
			attr2.setEType(EcorePackage.Literals.EINT);
			subEClass1.getEStructuralFeatures().add(attr2);

			EClass eClass2 = EcoreFactory.eINSTANCE.createEClass();
			eClass2.setName(ECLASS2_NAME);
			rootEPackage.getEClassifiers().add(eClass2);

			EPackage.Registry.INSTANCE.put(ROOT_EPACKAGE_NSURI, rootEPackage);
			EPackage.Registry.INSTANCE.put(SUB_EPACKAGE1_NSURI, subEPackage1);

			Resource r = new XMIResourceImpl();
			r.getContents().add(rootEPackage);
			try {
				System.out
						.println("--- rootEPackage-----------------------------");
				r.save(System.out, null);
				System.out
						.println("---------------------------------------------");
			} catch (IOException e) {
				e.printStackTrace();
			}
			r.getContents().clear();
			r.getContents().add(subEPackage1);
			try {
				System.out
				.println("--- subEPackage1-----------------------------");
				r.save(System.out, null);
				System.out
				.println("---------------------------------------------");
			} catch (IOException e) {
				e.printStackTrace();
			}

		}
		return rootEPackage;
	}

}
