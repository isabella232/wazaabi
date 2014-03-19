/**
 *  Copyright (c) 2014 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.impl;

import org.eclipse.e4.ui.model.application.impl.ApplicationPackageImpl;

import org.eclipse.e4.ui.model.application.ui.basic.impl.BasicPackageImpl;

import org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.WazaabiE4Factory;
import org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.WazaabiE4Package;
import org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.WazaabiPart;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class WazaabiE4PackageImpl extends EPackageImpl implements WazaabiE4Package {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass wazaabiPartEClass = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.WazaabiE4Package#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private WazaabiE4PackageImpl() {
		super(eNS_URI, WazaabiE4Factory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 * 
	 * <p>This method is used to initialize {@link WazaabiE4Package#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static WazaabiE4Package init() {
		if (isInited) return (WazaabiE4Package)EPackage.Registry.INSTANCE.getEPackage(WazaabiE4Package.eNS_URI);

		// Obtain or create and register package
		WazaabiE4PackageImpl theWazaabiE4Package = (WazaabiE4PackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof WazaabiE4PackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new WazaabiE4PackageImpl());

		isInited = true;

		// Initialize simple dependencies
		ApplicationPackageImpl.eINSTANCE.eClass();

		// Create package meta-data objects
		theWazaabiE4Package.createPackageContents();

		// Initialize created meta-data
		theWazaabiE4Package.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theWazaabiE4Package.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(WazaabiE4Package.eNS_URI, theWazaabiE4Package);
		return theWazaabiE4Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getWazaabiPart() {
		return wazaabiPartEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public WazaabiE4Factory getWazaabiE4Factory() {
		return (WazaabiE4Factory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		wazaabiPartEClass = createEClass(WAZAABI_PART);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		BasicPackageImpl theBasicPackage = (BasicPackageImpl)EPackage.Registry.INSTANCE.getEPackage(BasicPackageImpl.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		wazaabiPartEClass.getESuperTypes().add(theBasicPackage.getPart());

		// Initialize classes, features, and operations; add parameters
		initEClass(wazaabiPartEClass, WazaabiPart.class, "WazaabiPart", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		// Create resource
		createResource(eNS_URI);
	}

} //WazaabiE4PackageImpl
