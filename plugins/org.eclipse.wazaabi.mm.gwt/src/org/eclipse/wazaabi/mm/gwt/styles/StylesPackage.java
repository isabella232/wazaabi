/**
 */
package org.eclipse.wazaabi.mm.gwt.styles;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.gwt.styles.StylesFactory
 * @model kind="package"
 * @generated
 */
public interface StylesPackage extends EPackage {
    /**
     * The package name.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    String eNAME = "styles";

    /**
     * The package namespace URI.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    String eNS_URI = "http://www.wazaabi.org/gwt/styles";

    /**
     * The package namespace name.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    String eNS_PREFIX = "fxstyles";

    /**
     * The singleton instance of the package.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    StylesPackage eINSTANCE = org.eclipse.wazaabi.mm.gwt.styles.impl.StylesPackageImpl.init();

    /**
     * The meta object id for the '{@link org.eclipse.wazaabi.mm.gwt.styles.impl.FlowLayoutRuleImpl <em>Flow Layout Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.gwt.styles.impl.FlowLayoutRuleImpl
     * @see org.eclipse.wazaabi.mm.gwt.styles.impl.StylesPackageImpl#getFlowLayoutRule()
     * @generated
     */
    int FLOW_LAYOUT_RULE = 0;

    /**
     * The feature id for the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int FLOW_LAYOUT_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

    /**
     * The number of structural features of the '<em>Flow Layout Rule</em>' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int FLOW_LAYOUT_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

    /**
     * The meta object id for the '{@link org.eclipse.wazaabi.mm.gwt.styles.impl.GridRuleImpl <em>Grid Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.gwt.styles.impl.GridRuleImpl
     * @see org.eclipse.wazaabi.mm.gwt.styles.impl.StylesPackageImpl#getGridRule()
     * @generated
     */
    int GRID_RULE = 1;

    /**
     * The feature id for the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int GRID_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

    /**
     * The number of structural features of the '<em>Grid Rule</em>' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int GRID_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;


    /**
     * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.gwt.styles.FlowLayoutRule <em>Flow Layout Rule</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for class '<em>Flow Layout Rule</em>'.
     * @see org.eclipse.wazaabi.mm.gwt.styles.FlowLayoutRule
     * @generated
     */
    EClass getFlowLayoutRule();

    /**
     * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.gwt.styles.GridRule <em>Grid Rule</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for class '<em>Grid Rule</em>'.
     * @see org.eclipse.wazaabi.mm.gwt.styles.GridRule
     * @generated
     */
    EClass getGridRule();

    /**
     * Returns the factory that creates the instances of the model.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the factory that creates the instances of the model.
     * @generated
     */
    StylesFactory getStylesFactory();

    /**
     * <!-- begin-user-doc -->
     * Defines literals for the meta objects that represent
     * <ul>
     *   <li>each class,</li>
     *   <li>each feature of each class,</li>
     *   <li>each enum,</li>
     *   <li>and each data type</li>
     * </ul>
     * <!-- end-user-doc -->
     * @generated
     */
    interface Literals {
        /**
         * The meta object literal for the '{@link org.eclipse.wazaabi.mm.gwt.styles.impl.FlowLayoutRuleImpl <em>Flow Layout Rule</em>}' class.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @see org.eclipse.wazaabi.mm.gwt.styles.impl.FlowLayoutRuleImpl
         * @see org.eclipse.wazaabi.mm.gwt.styles.impl.StylesPackageImpl#getFlowLayoutRule()
         * @generated
         */
        EClass FLOW_LAYOUT_RULE = eINSTANCE.getFlowLayoutRule();

        /**
         * The meta object literal for the '{@link org.eclipse.wazaabi.mm.gwt.styles.impl.GridRuleImpl <em>Grid Rule</em>}' class.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @see org.eclipse.wazaabi.mm.gwt.styles.impl.GridRuleImpl
         * @see org.eclipse.wazaabi.mm.gwt.styles.impl.StylesPackageImpl#getGridRule()
         * @generated
         */
        EClass GRID_RULE = eINSTANCE.getGridRule();

    }

} //StylesPackage
