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
package org.eclipse.wazaabi.mm.core.styles.collections.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage;

import org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl;

import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;

import org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl;

import org.eclipse.wazaabi.mm.core.handlers.CoreHandlersPackage;

import org.eclipse.wazaabi.mm.core.handlers.impl.CoreHandlersPackageImpl;

import org.eclipse.wazaabi.mm.core.impl.CorePackageImpl;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;

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
public class CoreCollectionsStylesPackageImpl extends EPackageImpl implements CoreCollectionsStylesPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass lookAndFeelRuleEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass columnDescriptorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass pathSelectorEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass dynamicProviderEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum lookAndFeelEEnum = null;

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
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private CoreCollectionsStylesPackageImpl() {
		super(eNS_URI, CoreCollectionsStylesFactory.eINSTANCE);
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
	 * <p>This method is used to initialize {@link CoreCollectionsStylesPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static CoreCollectionsStylesPackage init() {
		if (isInited) return (CoreCollectionsStylesPackage)EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI);

		// Obtain or create and register package
		CoreCollectionsStylesPackageImpl theCoreCollectionsStylesPackage = (CoreCollectionsStylesPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof CoreCollectionsStylesPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new CoreCollectionsStylesPackageImpl());

		isInited = true;

		// Initialize simple dependencies
		EdpPackage.eINSTANCE.eClass();

		// Obtain or create and register interdependencies
		CorePackageImpl theCorePackage = (CorePackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) instanceof CorePackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) : CorePackage.eINSTANCE);
		CoreWidgetsPackageImpl theCoreWidgetsPackage = (CoreWidgetsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) instanceof CoreWidgetsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) : CoreWidgetsPackage.eINSTANCE);
		CoreStylesPackageImpl theCoreStylesPackage = (CoreStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) instanceof CoreStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI) : CoreStylesPackage.eINSTANCE);
		CoreAnnotationsPackageImpl theCoreAnnotationsPackage = (CoreAnnotationsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) instanceof CoreAnnotationsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) : CoreAnnotationsPackage.eINSTANCE);
		CoreHandlersPackageImpl theCoreHandlersPackage = (CoreHandlersPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) instanceof CoreHandlersPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) : CoreHandlersPackage.eINSTANCE);
		CoreExtrasPackageImpl theCoreExtrasPackage = (CoreExtrasPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) instanceof CoreExtrasPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) : CoreExtrasPackage.eINSTANCE);

		// Create package meta-data objects
		theCoreCollectionsStylesPackage.createPackageContents();
		theCorePackage.createPackageContents();
		theCoreWidgetsPackage.createPackageContents();
		theCoreStylesPackage.createPackageContents();
		theCoreAnnotationsPackage.createPackageContents();
		theCoreHandlersPackage.createPackageContents();
		theCoreExtrasPackage.createPackageContents();

		// Initialize created meta-data
		theCoreCollectionsStylesPackage.initializePackageContents();
		theCorePackage.initializePackageContents();
		theCoreWidgetsPackage.initializePackageContents();
		theCoreStylesPackage.initializePackageContents();
		theCoreAnnotationsPackage.initializePackageContents();
		theCoreHandlersPackage.initializePackageContents();
		theCoreExtrasPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theCoreCollectionsStylesPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(CoreCollectionsStylesPackage.eNS_URI, theCoreCollectionsStylesPackage);
		return theCoreCollectionsStylesPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLookAndFeelRule() {
		return lookAndFeelRuleEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getLookAndFeelRule_Value() {
		return (EAttribute)lookAndFeelRuleEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getColumnDescriptor() {
		return columnDescriptorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnDescriptor_Label() {
		return (EAttribute)columnDescriptorEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnDescriptor_MinimumWidth() {
		return (EAttribute)columnDescriptorEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnDescriptor_EditingSupport() {
		return (EAttribute)columnDescriptorEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getColumnDescriptor_CellEditor() {
		return (EReference)columnDescriptorEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getPathSelector() {
		return pathSelectorEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getPathSelector_EClassifierName() {
		return (EAttribute)pathSelectorEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getPathSelector_Context() {
		return (EAttribute)pathSelectorEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getPathSelector_Paths() {
		return (EAttribute)pathSelectorEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getDynamicProvider() {
		return dynamicProviderEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getDynamicProvider_Uri() {
		return (EAttribute)dynamicProviderEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getLookAndFeel() {
		return lookAndFeelEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreCollectionsStylesFactory getCoreCollectionsStylesFactory() {
		return (CoreCollectionsStylesFactory)getEFactoryInstance();
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
		lookAndFeelRuleEClass = createEClass(LOOK_AND_FEEL_RULE);
		createEAttribute(lookAndFeelRuleEClass, LOOK_AND_FEEL_RULE__VALUE);

		columnDescriptorEClass = createEClass(COLUMN_DESCRIPTOR);
		createEAttribute(columnDescriptorEClass, COLUMN_DESCRIPTOR__LABEL);
		createEAttribute(columnDescriptorEClass, COLUMN_DESCRIPTOR__MINIMUM_WIDTH);
		createEAttribute(columnDescriptorEClass, COLUMN_DESCRIPTOR__EDITING_SUPPORT);
		createEReference(columnDescriptorEClass, COLUMN_DESCRIPTOR__CELL_EDITOR);

		pathSelectorEClass = createEClass(PATH_SELECTOR);
		createEAttribute(pathSelectorEClass, PATH_SELECTOR__ECLASSIFIER_NAME);
		createEAttribute(pathSelectorEClass, PATH_SELECTOR__CONTEXT);
		createEAttribute(pathSelectorEClass, PATH_SELECTOR__PATHS);

		dynamicProviderEClass = createEClass(DYNAMIC_PROVIDER);
		createEAttribute(dynamicProviderEClass, DYNAMIC_PROVIDER__URI);

		// Create enums
		lookAndFeelEEnum = createEEnum(LOOK_AND_FEEL);
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
		CoreStylesPackage theCoreStylesPackage = (CoreStylesPackage)EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI);
		EDPHandlersPackage theEDPHandlersPackage = (EDPHandlersPackage)EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI);
		CoreExtrasPackage theCoreExtrasPackage = (CoreExtrasPackage)EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		lookAndFeelRuleEClass.getESuperTypes().add(theCoreStylesPackage.getStyleRule());
		columnDescriptorEClass.getESuperTypes().add(theCoreStylesPackage.getStyleRule());
		columnDescriptorEClass.getESuperTypes().add(theEDPHandlersPackage.getParameterized());
		pathSelectorEClass.getESuperTypes().add(theCoreStylesPackage.getStyleRule());
		dynamicProviderEClass.getESuperTypes().add(theCoreStylesPackage.getStyleRule());

		// Initialize classes and features; add operations and parameters
		initEClass(lookAndFeelRuleEClass, LookAndFeelRule.class, "LookAndFeelRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getLookAndFeelRule_Value(), this.getLookAndFeel(), "value", "TABLE", 0, 1, LookAndFeelRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(columnDescriptorEClass, ColumnDescriptor.class, "ColumnDescriptor", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getColumnDescriptor_Label(), ecorePackage.getEString(), "label", null, 0, 1, ColumnDescriptor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getColumnDescriptor_MinimumWidth(), ecorePackage.getEInt(), "minimumWidth", "20", 0, 1, ColumnDescriptor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getColumnDescriptor_EditingSupport(), ecorePackage.getEString(), "editingSupport", null, 0, 1, ColumnDescriptor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getColumnDescriptor_CellEditor(), theCoreExtrasPackage.getCellEditor(), null, "cellEditor", null, 0, 1, ColumnDescriptor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(pathSelectorEClass, PathSelector.class, "PathSelector", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getPathSelector_EClassifierName(), ecorePackage.getEString(), "eClassifierName", null, 0, 1, PathSelector.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getPathSelector_Context(), ecorePackage.getEString(), "context", ".", 0, 1, PathSelector.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getPathSelector_Paths(), ecorePackage.getEString(), "paths", null, 0, -1, PathSelector.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(dynamicProviderEClass, DynamicProvider.class, "DynamicProvider", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getDynamicProvider_Uri(), ecorePackage.getEString(), "uri", null, 0, 1, DynamicProvider.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(lookAndFeelEEnum, LookAndFeel.class, "LookAndFeel");
		addEEnumLiteral(lookAndFeelEEnum, LookAndFeel.TABLE);
		addEEnumLiteral(lookAndFeelEEnum, LookAndFeel.TREE);
		addEEnumLiteral(lookAndFeelEEnum, LookAndFeel.COMBOBOX);
	}

} //CoreCollectionsStylesPackageImpl
