/**
 */
package org.eclipse.wazaabi.mm.fx.styles.util;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.fx.styles.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage
 * @generated
 */
public class FXStylesSwitch<T> extends Switch<T> {
    /**
     * The cached model package
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected static FXStylesPackage modelPackage;

    /**
     * Creates an instance of the switch.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public FXStylesSwitch() {
        if (modelPackage == null) {
            modelPackage = FXStylesPackage.eINSTANCE;
        }
    }

    /**
     * Checks whether this is a switch for the given package.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @parameter ePackage the package in question.
     * @return whether this is a switch for the given package.
     * @generated
     */
    @Override
    protected boolean isSwitchFor(EPackage ePackage) {
        return ePackage == modelPackage;
    }

    /**
     * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the first non-null result returned by a <code>caseXXX</code> call.
     * @generated
     */
    @Override
    protected T doSwitch(int classifierID, EObject theEObject) {
        switch (classifierID) {
            case FXStylesPackage.BORDER_LAYOUT_RULE: {
                BorderLayoutRule borderLayoutRule = (BorderLayoutRule)theEObject;
                T result = caseBorderLayoutRule(borderLayoutRule);
                if (result == null) result = caseLayoutRule(borderLayoutRule);
                if (result == null) result = caseStyleRule(borderLayoutRule);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case FXStylesPackage.HBOX_RULE: {
                HBoxRule hBoxRule = (HBoxRule)theEObject;
                T result = caseHBoxRule(hBoxRule);
                if (result == null) result = caseLayoutRule(hBoxRule);
                if (result == null) result = caseStyleRule(hBoxRule);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case FXStylesPackage.VBOX_RULE: {
                VBoxRule vBoxRule = (VBoxRule)theEObject;
                T result = caseVBoxRule(vBoxRule);
                if (result == null) result = caseLayoutRule(vBoxRule);
                if (result == null) result = caseStyleRule(vBoxRule);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case FXStylesPackage.BORDER_LAYOUT_DATA: {
                BorderLayoutData borderLayoutData = (BorderLayoutData)theEObject;
                T result = caseBorderLayoutData(borderLayoutData);
                if (result == null) result = caseLayoutDataRule(borderLayoutData);
                if (result == null) result = caseStyleRule(borderLayoutData);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            default: return defaultCase(theEObject);
        }
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>Border Layout Rule</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Border Layout Rule</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
    public T caseBorderLayoutRule(BorderLayoutRule object) {
        return null;
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>HBox Rule</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>HBox Rule</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
    public T caseHBoxRule(HBoxRule object) {
        return null;
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>VBox Rule</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>VBox Rule</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
    public T caseVBoxRule(VBoxRule object) {
        return null;
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>Border Layout Data</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Border Layout Data</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
    public T caseBorderLayoutData(BorderLayoutData object) {
        return null;
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>Style Rule</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Style Rule</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
    public T caseStyleRule(StyleRule object) {
        return null;
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>Layout Rule</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Layout Rule</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
    public T caseLayoutRule(LayoutRule object) {
        return null;
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>Layout Data Rule</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Layout Data Rule</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
    public T caseLayoutDataRule(LayoutDataRule object) {
        return null;
    }

    /**
     * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
     * <!-- begin-user-doc -->
     * This implementation returns null;
     * returning a non-null result will terminate the switch, but this is the last case anyway.
     * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject)
     * @generated
     */
    @Override
    public T defaultCase(EObject object) {
        return null;
    }

} //FXStylesSwitch
