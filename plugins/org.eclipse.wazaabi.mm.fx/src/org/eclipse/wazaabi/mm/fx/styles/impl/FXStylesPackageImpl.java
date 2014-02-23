/**
 */
package org.eclipse.wazaabi.mm.fx.styles.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

import org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData;
import org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition;
import org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesFactory;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage;
import org.eclipse.wazaabi.mm.fx.styles.HBoxRule;
import org.eclipse.wazaabi.mm.fx.styles.VBoxRule;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class FXStylesPackageImpl extends EPackageImpl implements FXStylesPackage {
    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    private EClass borderLayoutRuleEClass = null;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    private EClass hBoxRuleEClass = null;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    private EClass vBoxRuleEClass = null;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    private EClass borderLayoutDataEClass = null;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    private EEnum borderLayoutPositionEEnum = null;

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
     * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage#eNS_URI
     * @see #init()
     * @generated
     */
    private FXStylesPackageImpl() {
        super(eNS_URI, FXStylesFactory.eINSTANCE);
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
     * <p>This method is used to initialize {@link FXStylesPackage#eINSTANCE} when that field is accessed.
     * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #eNS_URI
     * @see #createPackageContents()
     * @see #initializePackageContents()
     * @generated
     */
    public static FXStylesPackage init() {
        if (isInited) return (FXStylesPackage)EPackage.Registry.INSTANCE.getEPackage(FXStylesPackage.eNS_URI);

        // Obtain or create and register package
        FXStylesPackageImpl theFXStylesPackage = (FXStylesPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof FXStylesPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new FXStylesPackageImpl());

        isInited = true;

        // Initialize simple dependencies
        CorePackage.eINSTANCE.eClass();

        // Create package meta-data objects
        theFXStylesPackage.createPackageContents();

        // Initialize created meta-data
        theFXStylesPackage.initializePackageContents();

        // Mark meta-data to indicate it can't be changed
        theFXStylesPackage.freeze();

  
        // Update the registry and return the package
        EPackage.Registry.INSTANCE.put(FXStylesPackage.eNS_URI, theFXStylesPackage);
        return theFXStylesPackage;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EClass getBorderLayoutRule() {
        return borderLayoutRuleEClass;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getBorderLayoutRule_Margin() {
        return (EAttribute)borderLayoutRuleEClass.getEStructuralFeatures().get(0);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EClass getHBoxRule() {
        return hBoxRuleEClass;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getHBoxRule_Spacing() {
        return (EAttribute)hBoxRuleEClass.getEStructuralFeatures().get(0);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getHBoxRule_Margin() {
        return (EAttribute)hBoxRuleEClass.getEStructuralFeatures().get(1);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EClass getVBoxRule() {
        return vBoxRuleEClass;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getVBoxRule_Spacing() {
        return (EAttribute)vBoxRuleEClass.getEStructuralFeatures().get(0);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getVBoxRule_Margin() {
        return (EAttribute)vBoxRuleEClass.getEStructuralFeatures().get(1);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EClass getBorderLayoutData() {
        return borderLayoutDataEClass;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getBorderLayoutData_Position() {
        return (EAttribute)borderLayoutDataEClass.getEStructuralFeatures().get(0);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EEnum getBorderLayoutPosition() {
        return borderLayoutPositionEEnum;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public FXStylesFactory getFXStylesFactory() {
        return (FXStylesFactory)getEFactoryInstance();
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
        borderLayoutRuleEClass = createEClass(BORDER_LAYOUT_RULE);
        createEAttribute(borderLayoutRuleEClass, BORDER_LAYOUT_RULE__MARGIN);

        hBoxRuleEClass = createEClass(HBOX_RULE);
        createEAttribute(hBoxRuleEClass, HBOX_RULE__SPACING);
        createEAttribute(hBoxRuleEClass, HBOX_RULE__MARGIN);

        vBoxRuleEClass = createEClass(VBOX_RULE);
        createEAttribute(vBoxRuleEClass, VBOX_RULE__SPACING);
        createEAttribute(vBoxRuleEClass, VBOX_RULE__MARGIN);

        borderLayoutDataEClass = createEClass(BORDER_LAYOUT_DATA);
        createEAttribute(borderLayoutDataEClass, BORDER_LAYOUT_DATA__POSITION);

        // Create enums
        borderLayoutPositionEEnum = createEEnum(BORDER_LAYOUT_POSITION);
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

        // Create type parameters

        // Set bounds for type parameters

        // Add supertypes to classes
        borderLayoutRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutRule());
        hBoxRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutRule());
        vBoxRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutRule());
        borderLayoutDataEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutDataRule());

        // Initialize classes and features; add operations and parameters
        initEClass(borderLayoutRuleEClass, BorderLayoutRule.class, "BorderLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getBorderLayoutRule_Margin(), ecorePackage.getEInt(), "margin", "10", 0, 1, BorderLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(hBoxRuleEClass, HBoxRule.class, "HBoxRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getHBoxRule_Spacing(), ecorePackage.getEInt(), "spacing", "10", 0, 1, HBoxRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getHBoxRule_Margin(), ecorePackage.getEInt(), "margin", "10", 0, 1, HBoxRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(vBoxRuleEClass, VBoxRule.class, "VBoxRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getVBoxRule_Spacing(), ecorePackage.getEInt(), "spacing", "10", 0, 1, VBoxRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getVBoxRule_Margin(), ecorePackage.getEInt(), "margin", "10", 0, 1, VBoxRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(borderLayoutDataEClass, BorderLayoutData.class, "BorderLayoutData", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getBorderLayoutData_Position(), this.getBorderLayoutPosition(), "position", "CENTER", 0, 1, BorderLayoutData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        // Initialize enums and add enum literals
        initEEnum(borderLayoutPositionEEnum, BorderLayoutPosition.class, "BorderLayoutPosition");
        addEEnumLiteral(borderLayoutPositionEEnum, BorderLayoutPosition.CENTER);
        addEEnumLiteral(borderLayoutPositionEEnum, BorderLayoutPosition.TOP);
        addEEnumLiteral(borderLayoutPositionEEnum, BorderLayoutPosition.RIGHT);
        addEEnumLiteral(borderLayoutPositionEEnum, BorderLayoutPosition.LEFT);
        addEEnumLiteral(borderLayoutPositionEEnum, BorderLayoutPosition.BOTTOM);

        // Create resource
        createResource(eNS_URI);
    }

} //FXStylesPackageImpl
