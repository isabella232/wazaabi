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
package org.eclipse.wazaabi.mm.core.extras.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage;

import org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl;

import org.eclipse.wazaabi.mm.core.extras.CellEditor;
import org.eclipse.wazaabi.mm.core.extras.CheckboxCellEditor;
import org.eclipse.wazaabi.mm.core.extras.CoreExtrasFactory;
import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;
import org.eclipse.wazaabi.mm.core.extras.TextCellEditor;

import org.eclipse.wazaabi.mm.core.handlers.CoreHandlersPackage;

import org.eclipse.wazaabi.mm.core.handlers.impl.CoreHandlersPackageImpl;

import org.eclipse.wazaabi.mm.core.impl.CorePackageImpl;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl;

import org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

import org.eclipse.wazaabi.mm.core.widgets.impl.CoreWidgetsPackageImpl;

import org.eclipse.wazaabi.mm.edp.EdpPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreExtrasPackageImpl extends EPackageImpl implements CoreExtrasPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cellEditorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass textCellEditorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass checkboxCellEditorEClass = null;

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
	 * @see org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private CoreExtrasPackageImpl() {
		super(eNS_URI, CoreExtrasFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link CoreExtrasPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static CoreExtrasPackage init() {
		if (isInited) return (CoreExtrasPackage)EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI);

		// Obtain or create and register package
		CoreExtrasPackageImpl theCoreExtrasPackage = (CoreExtrasPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof CoreExtrasPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new CoreExtrasPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		EdpPackage.eINSTANCE.eClass();

		// Obtain or create and register interdependencies
		CorePackageImpl theCorePackage = (CorePackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) instanceof CorePackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) : CorePackage.eINSTANCE);
		CoreWidgetsPackageImpl theCoreWidgetsPackage = (CoreWidgetsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) instanceof CoreWidgetsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) : CoreWidgetsPackage.eINSTANCE);
		CoreStylesPackageImpl theCoreStylesPackage = (CoreStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) instanceof CoreStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) : CoreStylesPackage.eINSTANCE);
		CoreCollectionsStylesPackageImpl theCoreCollectionsStylesPackage = (CoreCollectionsStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) instanceof CoreCollectionsStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) : CoreCollectionsStylesPackage.eINSTANCE);
		CoreAnnotationsPackageImpl theCoreAnnotationsPackage = (CoreAnnotationsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) instanceof CoreAnnotationsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) : CoreAnnotationsPackage.eINSTANCE);
		CoreHandlersPackageImpl theCoreHandlersPackage = (CoreHandlersPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) instanceof CoreHandlersPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) : CoreHandlersPackage.eINSTANCE);

		// Create package meta-data objects
		theCoreExtrasPackage.createPackageContents();
		theCorePackage.createPackageContents();
		theCoreWidgetsPackage.createPackageContents();
		theCoreStylesPackage.createPackageContents();
		theCoreCollectionsStylesPackage.createPackageContents();
		theCoreAnnotationsPackage.createPackageContents();
		theCoreHandlersPackage.createPackageContents();

		// Initialize created meta-data
		theCoreExtrasPackage.initializePackageContents();
		theCorePackage.initializePackageContents();
		theCoreWidgetsPackage.initializePackageContents();
		theCoreStylesPackage.initializePackageContents();
		theCoreCollectionsStylesPackage.initializePackageContents();
		theCoreAnnotationsPackage.initializePackageContents();
		theCoreHandlersPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theCoreExtrasPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(CoreExtrasPackage.eNS_URI, theCoreExtrasPackage);
		return theCoreExtrasPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCellEditor() {
		return cellEditorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getTextCellEditor() {
		return textCellEditorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCheckboxCellEditor() {
		return checkboxCellEditorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreExtrasFactory getCoreExtrasFactory() {
		return (CoreExtrasFactory)getEFactoryInstance();
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
		cellEditorEClass = createEClass(CELL_EDITOR);

		textCellEditorEClass = createEClass(TEXT_CELL_EDITOR);

		checkboxCellEditorEClass = createEClass(CHECKBOX_CELL_EDITOR);
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

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		textCellEditorEClass.getESuperTypes().add(this.getCellEditor());
		checkboxCellEditorEClass.getESuperTypes().add(this.getCellEditor());

		// Initialize classes and features; add operations and parameters
		initEClass(cellEditorEClass, CellEditor.class, "CellEditor", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(textCellEditorEClass, TextCellEditor.class, "TextCellEditor", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(checkboxCellEditorEClass, CheckboxCellEditor.class, "CheckboxCellEditor", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
	}

} //CoreExtrasPackageImpl
