/**
 */
package org.eclipse.wazaabi.mm.fx.styles;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesFactory
 * @model kind="package"
 * @generated
 */
public interface FXStylesPackage extends EPackage {
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
    String eNS_URI = "http://www.wazaabi.org/fx/styles";

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
    FXStylesPackage eINSTANCE = org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl.init();

    /**
     * The meta object id for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutRuleImpl <em>Border Layout Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutRuleImpl
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getBorderLayoutRule()
     * @generated
     */
    int BORDER_LAYOUT_RULE = 0;

    /**
     * The feature id for the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int BORDER_LAYOUT_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

    /**
     * The feature id for the '<em><b>Margin</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int BORDER_LAYOUT_RULE__MARGIN = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

    /**
     * The number of structural features of the '<em>Border Layout Rule</em>' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int BORDER_LAYOUT_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 1;

    /**
     * The meta object id for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.HBoxRuleImpl <em>HBox Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.HBoxRuleImpl
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getHBoxRule()
     * @generated
     */
    int HBOX_RULE = 1;

    /**
     * The feature id for the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int HBOX_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

    /**
     * The feature id for the '<em><b>Spacing</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int HBOX_RULE__SPACING = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

    /**
     * The feature id for the '<em><b>Margin</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int HBOX_RULE__MARGIN = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 1;

    /**
     * The number of structural features of the '<em>HBox Rule</em>' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int HBOX_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 2;

    /**
     * The meta object id for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.VBoxRuleImpl <em>VBox Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.VBoxRuleImpl
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getVBoxRule()
     * @generated
     */
    int VBOX_RULE = 2;

    /**
     * The feature id for the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int VBOX_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

    /**
     * The feature id for the '<em><b>Spacing</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int VBOX_RULE__SPACING = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

    /**
     * The feature id for the '<em><b>Margin</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int VBOX_RULE__MARGIN = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 1;

    /**
     * The number of structural features of the '<em>VBox Rule</em>' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int VBOX_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 2;

    /**
     * The meta object id for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutDataImpl <em>Border Layout Data</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutDataImpl
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getBorderLayoutData()
     * @generated
     */
    int BORDER_LAYOUT_DATA = 3;

    /**
     * The feature id for the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int BORDER_LAYOUT_DATA__PROPERTY_NAME = CoreStylesPackage.LAYOUT_DATA_RULE__PROPERTY_NAME;

    /**
     * The feature id for the '<em><b>Position</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int BORDER_LAYOUT_DATA__POSITION = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 0;

    /**
     * The number of structural features of the '<em>Border Layout Data</em>' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
    int BORDER_LAYOUT_DATA_FEATURE_COUNT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 1;

    /**
     * The meta object id for the '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition <em>Border Layout Position</em>}' enum.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition
     * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getBorderLayoutPosition()
     * @generated
     */
    int BORDER_LAYOUT_POSITION = 4;


    /**
     * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule <em>Border Layout Rule</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for class '<em>Border Layout Rule</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule
     * @generated
     */
    EClass getBorderLayoutRule();

    /**
     * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule#getMargin <em>Margin</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Margin</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule#getMargin()
     * @see #getBorderLayoutRule()
     * @generated
     */
    EAttribute getBorderLayoutRule_Margin();

    /**
     * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.fx.styles.HBoxRule <em>HBox Rule</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for class '<em>HBox Rule</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.HBoxRule
     * @generated
     */
    EClass getHBoxRule();

    /**
     * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.fx.styles.HBoxRule#getSpacing <em>Spacing</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Spacing</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.HBoxRule#getSpacing()
     * @see #getHBoxRule()
     * @generated
     */
    EAttribute getHBoxRule_Spacing();

    /**
     * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.fx.styles.HBoxRule#getMargin <em>Margin</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Margin</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.HBoxRule#getMargin()
     * @see #getHBoxRule()
     * @generated
     */
    EAttribute getHBoxRule_Margin();

    /**
     * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.fx.styles.VBoxRule <em>VBox Rule</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for class '<em>VBox Rule</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.VBoxRule
     * @generated
     */
    EClass getVBoxRule();

    /**
     * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.fx.styles.VBoxRule#getSpacing <em>Spacing</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Spacing</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.VBoxRule#getSpacing()
     * @see #getVBoxRule()
     * @generated
     */
    EAttribute getVBoxRule_Spacing();

    /**
     * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.fx.styles.VBoxRule#getMargin <em>Margin</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Margin</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.VBoxRule#getMargin()
     * @see #getVBoxRule()
     * @generated
     */
    EAttribute getVBoxRule_Margin();

    /**
     * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData <em>Border Layout Data</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for class '<em>Border Layout Data</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData
     * @generated
     */
    EClass getBorderLayoutData();

    /**
     * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData#getPosition <em>Position</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for the attribute '<em>Position</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData#getPosition()
     * @see #getBorderLayoutData()
     * @generated
     */
    EAttribute getBorderLayoutData_Position();

    /**
     * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition <em>Border Layout Position</em>}'.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the meta object for enum '<em>Border Layout Position</em>'.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition
     * @generated
     */
    EEnum getBorderLayoutPosition();

    /**
     * Returns the factory that creates the instances of the model.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @return the factory that creates the instances of the model.
     * @generated
     */
    FXStylesFactory getFXStylesFactory();

    /**
     * <!-- begin-user-doc -->
     * Defines literals for the meta objects that represent
     * <ul>
     *   <li>each class,</li>
     *   <li>each feature of each class,</li>
     *   <li>each operation of each class,</li>
     *   <li>each enum,</li>
     *   <li>and each data type</li>
     * </ul>
     * <!-- end-user-doc -->
     * @generated
     */
    interface Literals {
        /**
         * The meta object literal for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutRuleImpl <em>Border Layout Rule</em>}' class.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutRuleImpl
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getBorderLayoutRule()
         * @generated
         */
        EClass BORDER_LAYOUT_RULE = eINSTANCE.getBorderLayoutRule();

        /**
         * The meta object literal for the '<em><b>Margin</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute BORDER_LAYOUT_RULE__MARGIN = eINSTANCE.getBorderLayoutRule_Margin();

        /**
         * The meta object literal for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.HBoxRuleImpl <em>HBox Rule</em>}' class.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.HBoxRuleImpl
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getHBoxRule()
         * @generated
         */
        EClass HBOX_RULE = eINSTANCE.getHBoxRule();

        /**
         * The meta object literal for the '<em><b>Spacing</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute HBOX_RULE__SPACING = eINSTANCE.getHBoxRule_Spacing();

        /**
         * The meta object literal for the '<em><b>Margin</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute HBOX_RULE__MARGIN = eINSTANCE.getHBoxRule_Margin();

        /**
         * The meta object literal for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.VBoxRuleImpl <em>VBox Rule</em>}' class.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.VBoxRuleImpl
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getVBoxRule()
         * @generated
         */
        EClass VBOX_RULE = eINSTANCE.getVBoxRule();

        /**
         * The meta object literal for the '<em><b>Spacing</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute VBOX_RULE__SPACING = eINSTANCE.getVBoxRule_Spacing();

        /**
         * The meta object literal for the '<em><b>Margin</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute VBOX_RULE__MARGIN = eINSTANCE.getVBoxRule_Margin();

        /**
         * The meta object literal for the '{@link org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutDataImpl <em>Border Layout Data</em>}' class.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutDataImpl
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getBorderLayoutData()
         * @generated
         */
        EClass BORDER_LAYOUT_DATA = eINSTANCE.getBorderLayoutData();

        /**
         * The meta object literal for the '<em><b>Position</b></em>' attribute feature.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @generated
         */
        EAttribute BORDER_LAYOUT_DATA__POSITION = eINSTANCE.getBorderLayoutData_Position();

        /**
         * The meta object literal for the '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition <em>Border Layout Position</em>}' enum.
         * <!-- begin-user-doc -->
         * <!-- end-user-doc -->
         * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition
         * @see org.eclipse.wazaabi.mm.fx.styles.impl.FXStylesPackageImpl#getBorderLayoutPosition()
         * @generated
         */
        EEnum BORDER_LAYOUT_POSITION = eINSTANCE.getBorderLayoutPosition();

    }

} //FXStylesPackage
