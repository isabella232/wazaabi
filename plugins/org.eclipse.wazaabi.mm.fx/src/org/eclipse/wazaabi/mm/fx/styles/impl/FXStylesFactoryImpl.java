/**
 */
package org.eclipse.wazaabi.mm.fx.styles.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.fx.styles.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class FXStylesFactoryImpl extends EFactoryImpl implements FXStylesFactory {
    /**
     * Creates the default factory implementation.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public static FXStylesFactory init() {
        try {
            FXStylesFactory theFXStylesFactory = (FXStylesFactory)EPackage.Registry.INSTANCE.getEFactory(FXStylesPackage.eNS_URI);
            if (theFXStylesFactory != null) {
                return theFXStylesFactory;
            }
        }
        catch (Exception exception) {
            EcorePlugin.INSTANCE.log(exception);
        }
        return new FXStylesFactoryImpl();
    }

    /**
     * Creates an instance of the factory.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public FXStylesFactoryImpl() {
        super();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public EObject create(EClass eClass) {
        switch (eClass.getClassifierID()) {
            case FXStylesPackage.BORDER_LAYOUT_RULE: return createBorderLayoutRule();
            case FXStylesPackage.HBOX_RULE: return createHBoxRule();
            case FXStylesPackage.VBOX_RULE: return createVBoxRule();
            case FXStylesPackage.BORDER_LAYOUT_DATA: return createBorderLayoutData();
            default:
                throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
        }
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public Object createFromString(EDataType eDataType, String initialValue) {
        switch (eDataType.getClassifierID()) {
            case FXStylesPackage.BORDER_LAYOUT_POSITION:
                return createBorderLayoutPositionFromString(eDataType, initialValue);
            default:
                throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
        }
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public String convertToString(EDataType eDataType, Object instanceValue) {
        switch (eDataType.getClassifierID()) {
            case FXStylesPackage.BORDER_LAYOUT_POSITION:
                return convertBorderLayoutPositionToString(eDataType, instanceValue);
            default:
                throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
        }
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public BorderLayoutRule createBorderLayoutRule() {
        BorderLayoutRuleImpl borderLayoutRule = new BorderLayoutRuleImpl();
        return borderLayoutRule;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public HBoxRule createHBoxRule() {
        HBoxRuleImpl hBoxRule = new HBoxRuleImpl();
        return hBoxRule;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public VBoxRule createVBoxRule() {
        VBoxRuleImpl vBoxRule = new VBoxRuleImpl();
        return vBoxRule;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public BorderLayoutData createBorderLayoutData() {
        BorderLayoutDataImpl borderLayoutData = new BorderLayoutDataImpl();
        return borderLayoutData;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public BorderLayoutPosition createBorderLayoutPositionFromString(EDataType eDataType, String initialValue) {
        BorderLayoutPosition result = BorderLayoutPosition.get(initialValue);
        if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
        return result;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public String convertBorderLayoutPositionToString(EDataType eDataType, Object instanceValue) {
        return instanceValue == null ? null : instanceValue.toString();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public FXStylesPackage getFXStylesPackage() {
        return (FXStylesPackage)getEPackage();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @deprecated
     * @generated
     */
    @Deprecated
    public static FXStylesPackage getPackage() {
        return FXStylesPackage.eINSTANCE;
    }

} //FXStylesFactoryImpl
