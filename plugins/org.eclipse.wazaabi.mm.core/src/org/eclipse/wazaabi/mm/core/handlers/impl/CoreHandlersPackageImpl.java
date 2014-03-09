/**
 *  Copyright (c) 2008 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.wazaabi.mm.core.handlers.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage;

import org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl;

import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;

import org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl;

import org.eclipse.wazaabi.mm.core.handlers.CoreHandlersFactory;
import org.eclipse.wazaabi.mm.core.handlers.CoreHandlersPackage;
import org.eclipse.wazaabi.mm.core.handlers.RefreshAction;

import org.eclipse.wazaabi.mm.core.impl.CorePackageImpl;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl;

import org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

import org.eclipse.wazaabi.mm.core.widgets.impl.CoreWidgetsPackageImpl;

import org.eclipse.wazaabi.mm.edp.EdpPackage;

import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreHandlersPackageImpl extends EPackageImpl implements CoreHandlersPackage {
	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass refreshActionEClass = null;

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
     * @see org.eclipse.wazaabi.mm.core.handlers.CoreHandlersPackage#eNS_URI
     * @see #init()
     * @generated
     */
	private CoreHandlersPackageImpl() {
        super(eNS_URI, CoreHandlersFactory.eINSTANCE);
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
     * <p>This method is used to initialize {@link CoreHandlersPackage#eINSTANCE} when that field is accessed.
     * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #eNS_URI
     * @see #createPackageContents()
     * @see #initializePackageContents()
     * @generated
     */
	public static CoreHandlersPackage init() {
        if (isInited) return (CoreHandlersPackage)EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI);

        // Obtain or create and register package
        CoreHandlersPackageImpl theCoreHandlersPackage = (CoreHandlersPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof CoreHandlersPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new CoreHandlersPackageImpl());

        isInited = true;

        // Initialize simple dependencies
        EdpPackage.eINSTANCE.eClass();

        // Obtain or create and register interdependencies
        CorePackageImpl theCorePackage = (CorePackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) instanceof CorePackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) : CorePackage.eINSTANCE);
        CoreWidgetsPackageImpl theCoreWidgetsPackage = (CoreWidgetsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) instanceof CoreWidgetsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) : CoreWidgetsPackage.eINSTANCE);
        CoreStylesPackageImpl theCoreStylesPackage = (CoreStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) instanceof CoreStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) : CoreStylesPackage.eINSTANCE);
        CoreCollectionsStylesPackageImpl theCoreCollectionsStylesPackage = (CoreCollectionsStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) instanceof CoreCollectionsStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) : CoreCollectionsStylesPackage.eINSTANCE);
        CoreAnnotationsPackageImpl theCoreAnnotationsPackage = (CoreAnnotationsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) instanceof CoreAnnotationsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) : CoreAnnotationsPackage.eINSTANCE);
        CoreExtrasPackageImpl theCoreExtrasPackage = (CoreExtrasPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) instanceof CoreExtrasPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) : CoreExtrasPackage.eINSTANCE);

        // Create package meta-data objects
        theCoreHandlersPackage.createPackageContents();
        theCorePackage.createPackageContents();
        theCoreWidgetsPackage.createPackageContents();
        theCoreStylesPackage.createPackageContents();
        theCoreCollectionsStylesPackage.createPackageContents();
        theCoreAnnotationsPackage.createPackageContents();
        theCoreExtrasPackage.createPackageContents();

        // Initialize created meta-data
        theCoreHandlersPackage.initializePackageContents();
        theCorePackage.initializePackageContents();
        theCoreWidgetsPackage.initializePackageContents();
        theCoreStylesPackage.initializePackageContents();
        theCoreCollectionsStylesPackage.initializePackageContents();
        theCoreAnnotationsPackage.initializePackageContents();
        theCoreExtrasPackage.initializePackageContents();

        // Mark meta-data to indicate it can't be changed
        theCoreHandlersPackage.freeze();

  
        // Update the registry and return the package
        EPackage.Registry.INSTANCE.put(CoreHandlersPackage.eNS_URI, theCoreHandlersPackage);
        return theCoreHandlersPackage;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getRefreshAction() {
        return refreshActionEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public CoreHandlersFactory getCoreHandlersFactory() {
        return (CoreHandlersFactory)getEFactoryInstance();
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
        refreshActionEClass = createEClass(REFRESH_ACTION);
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
        EDPHandlersPackage theEDPHandlersPackage = (EDPHandlersPackage)EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI);

        // Create type parameters

        // Set bounds for type parameters

        // Add supertypes to classes
        refreshActionEClass.getESuperTypes().add(theEDPHandlersPackage.getOperation());

        // Initialize classes and features; add operations and parameters
        initEClass(refreshActionEClass, RefreshAction.class, "RefreshAction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    }

} //CoreHandlersPackageImpl
