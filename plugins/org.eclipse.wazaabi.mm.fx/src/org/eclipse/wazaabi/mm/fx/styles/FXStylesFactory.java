/**
 */
package org.eclipse.wazaabi.mm.fx.styles;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage
 * @generated
 */
public interface FXStylesFactory extends EFactory {
    /**
     * The singleton instance of the factory.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    FXStylesFactory eINSTANCE = org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesFactoryImpl.init();

    /**
     * Returns a new object of class '<em>Border Layout Rule</em>'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return a new object of class '<em>Border Layout Rule</em>'.
     * @generated
     */
    BorderLayoutRule createBorderLayoutRule();

    /**
     * Returns a new object of class '<em>HBox Rule</em>'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return a new object of class '<em>HBox Rule</em>'.
     * @generated
     */
    HBoxRule createHBoxRule();

    /**
     * Returns a new object of class '<em>VBox Rule</em>'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return a new object of class '<em>VBox Rule</em>'.
     * @generated
     */
    VBoxRule createVBoxRule();

    /**
     * Returns a new object of class '<em>Border Layout Data</em>'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return a new object of class '<em>Border Layout Data</em>'.
     * @generated
     */
    BorderLayoutData createBorderLayoutData();

    /**
     * Returns the package supported by this factory.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the package supported by this factory.
     * @generated
     */
    FXStylesPackage getFXStylesPackage();

} //FXStylesFactory
