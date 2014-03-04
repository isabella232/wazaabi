/**
 */
package org.eclipse.wazaabi.mm.gwt.styles;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.gwt.styles.StylesPackage
 * @generated
 */
public interface StylesFactory extends EFactory {
    /**
     * The singleton instance of the factory.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    StylesFactory eINSTANCE = org.eclipse.wazaabi.mm.gwt.styles.impl.StylesFactoryImpl.init();

    /**
     * Returns a new object of class '<em>Flow Layout Rule</em>'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return a new object of class '<em>Flow Layout Rule</em>'.
     * @generated
     */
    FlowLayoutRule createFlowLayoutRule();

    /**
     * Returns a new object of class '<em>Grid Rule</em>'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return a new object of class '<em>Grid Rule</em>'.
     * @generated
     */
    GridRule createGridRule();

    /**
     * Returns the package supported by this factory.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the package supported by this factory.
     * @generated
     */
    StylesPackage getStylesPackage();

} //StylesFactory
