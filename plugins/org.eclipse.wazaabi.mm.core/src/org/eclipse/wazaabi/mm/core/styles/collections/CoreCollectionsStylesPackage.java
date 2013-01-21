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
package org.eclipse.wazaabi.mm.core.styles.collections;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

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
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory
 * @model kind="package"
 * @generated
 */
public interface CoreCollectionsStylesPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "collections";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/core/styles/collections";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "wcscol";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CoreCollectionsStylesPackage eINSTANCE = org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.LookAndFeelRuleImpl <em>Look And Feel Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.LookAndFeelRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getLookAndFeelRule()
	 * @generated
	 */
	int LOOK_AND_FEEL_RULE = 0;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOK_AND_FEEL_RULE__PROPERTY_NAME = CoreStylesPackage.STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOK_AND_FEEL_RULE__VALUE = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Look And Feel Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOK_AND_FEEL_RULE_FEATURE_COUNT = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl <em>Abstract Column Descriptor</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getAbstractColumnDescriptor()
	 * @generated
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR = 1;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME = CoreStylesPackage.STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Label</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR__LABEL = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Editing Support</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Cell Editor</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 3;

	/**
	 * The feature id for the '<em><b>Resizable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 4;

	/**
	 * The number of structural features of the '<em>Abstract Column Descriptor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_COLUMN_DESCRIPTOR_FEATURE_COUNT = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 5;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl <em>Path Selector</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getPathSelector()
	 * @generated
	 */
	int PATH_SELECTOR = 2;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PATH_SELECTOR__PROPERTY_NAME = CoreStylesPackage.STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>EClassifier Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PATH_SELECTOR__ECLASSIFIER_NAME = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Context</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PATH_SELECTOR__CONTEXT = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Paths</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PATH_SELECTOR__PATHS = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Path Selector</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PATH_SELECTOR_FEATURE_COUNT = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.DynamicProviderImpl <em>Dynamic Provider</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.DynamicProviderImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getDynamicProvider()
	 * @generated
	 */
	int DYNAMIC_PROVIDER = 3;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DYNAMIC_PROVIDER__PROPERTY_NAME = CoreStylesPackage.STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Uri</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DYNAMIC_PROVIDER__URI = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Dynamic Provider</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DYNAMIC_PROVIDER_FEATURE_COUNT = CoreStylesPackage.STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl <em>Column Descriptor</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getColumnDescriptor()
	 * @generated
	 */
	int COLUMN_DESCRIPTOR = 4;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR__PROPERTY_NAME = ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR__PARAMETERS = ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS;

	/**
	 * The feature id for the '<em><b>Label</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR__LABEL = ABSTRACT_COLUMN_DESCRIPTOR__LABEL;

	/**
	 * The feature id for the '<em><b>Editing Support</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR__EDITING_SUPPORT = ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT;

	/**
	 * The feature id for the '<em><b>Cell Editor</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR__CELL_EDITOR = ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR;

	/**
	 * The feature id for the '<em><b>Resizable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR__RESIZABLE = ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE;

	/**
	 * The feature id for the '<em><b>Width</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR__WIDTH = ABSTRACT_COLUMN_DESCRIPTOR_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Column Descriptor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_DESCRIPTOR_FEATURE_COUNT = ABSTRACT_COLUMN_DESCRIPTOR_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.WeightedColumnDescriptorImpl <em>Weighted Column Descriptor</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.WeightedColumnDescriptorImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getWeightedColumnDescriptor()
	 * @generated
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR = 5;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__PROPERTY_NAME = ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__PARAMETERS = ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS;

	/**
	 * The feature id for the '<em><b>Label</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__LABEL = ABSTRACT_COLUMN_DESCRIPTOR__LABEL;

	/**
	 * The feature id for the '<em><b>Editing Support</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__EDITING_SUPPORT = ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT;

	/**
	 * The feature id for the '<em><b>Cell Editor</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__CELL_EDITOR = ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR;

	/**
	 * The feature id for the '<em><b>Resizable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__RESIZABLE = ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE;

	/**
	 * The feature id for the '<em><b>Weight</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__WEIGHT = ABSTRACT_COLUMN_DESCRIPTOR_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Minimum Width</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR__MINIMUM_WIDTH = ABSTRACT_COLUMN_DESCRIPTOR_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Weighted Column Descriptor</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WEIGHTED_COLUMN_DESCRIPTOR_FEATURE_COUNT = ABSTRACT_COLUMN_DESCRIPTOR_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel <em>Look And Feel</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getLookAndFeel()
	 * @generated
	 */
	int LOOK_AND_FEEL = 6;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule <em>Look And Feel Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Look And Feel Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule
	 * @generated
	 */
	EClass getLookAndFeelRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule#getValue()
	 * @see #getLookAndFeelRule()
	 * @generated
	 */
	EAttribute getLookAndFeelRule_Value();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor <em>Abstract Column Descriptor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Abstract Column Descriptor</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor
	 * @generated
	 */
	EClass getAbstractColumnDescriptor();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getLabel <em>Label</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Label</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getLabel()
	 * @see #getAbstractColumnDescriptor()
	 * @generated
	 */
	EAttribute getAbstractColumnDescriptor_Label();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getEditingSupport <em>Editing Support</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Editing Support</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getEditingSupport()
	 * @see #getAbstractColumnDescriptor()
	 * @generated
	 */
	EAttribute getAbstractColumnDescriptor_EditingSupport();

	/**
	 * Returns the meta object for the containment reference '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getCellEditor <em>Cell Editor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Cell Editor</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#getCellEditor()
	 * @see #getAbstractColumnDescriptor()
	 * @generated
	 */
	EReference getAbstractColumnDescriptor_CellEditor();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#isResizable <em>Resizable</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Resizable</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor#isResizable()
	 * @see #getAbstractColumnDescriptor()
	 * @generated
	 */
	EAttribute getAbstractColumnDescriptor_Resizable();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector <em>Path Selector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Path Selector</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.PathSelector
	 * @generated
	 */
	EClass getPathSelector();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getEClassifierName <em>EClassifier Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>EClassifier Name</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getEClassifierName()
	 * @see #getPathSelector()
	 * @generated
	 */
	EAttribute getPathSelector_EClassifierName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getContext <em>Context</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Context</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getContext()
	 * @see #getPathSelector()
	 * @generated
	 */
	EAttribute getPathSelector_Context();

	/**
	 * Returns the meta object for the attribute list '{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getPaths <em>Paths</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Paths</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getPaths()
	 * @see #getPathSelector()
	 * @generated
	 */
	EAttribute getPathSelector_Paths();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider <em>Dynamic Provider</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Dynamic Provider</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider
	 * @generated
	 */
	EClass getDynamicProvider();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider#getUri <em>Uri</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uri</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider#getUri()
	 * @see #getDynamicProvider()
	 * @generated
	 */
	EAttribute getDynamicProvider_Uri();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor <em>Column Descriptor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Column Descriptor</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor
	 * @generated
	 */
	EClass getColumnDescriptor();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getWidth <em>Width</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Width</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor#getWidth()
	 * @see #getColumnDescriptor()
	 * @generated
	 */
	EAttribute getColumnDescriptor_Width();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor <em>Weighted Column Descriptor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Weighted Column Descriptor</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor
	 * @generated
	 */
	EClass getWeightedColumnDescriptor();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getWeight <em>Weight</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Weight</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getWeight()
	 * @see #getWeightedColumnDescriptor()
	 * @generated
	 */
	EAttribute getWeightedColumnDescriptor_Weight();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getMinimumWidth <em>Minimum Width</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Minimum Width</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getMinimumWidth()
	 * @see #getWeightedColumnDescriptor()
	 * @generated
	 */
	EAttribute getWeightedColumnDescriptor_MinimumWidth();

	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel <em>Look And Feel</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Look And Feel</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel
	 * @generated
	 */
	EEnum getLookAndFeel();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CoreCollectionsStylesFactory getCoreCollectionsStylesFactory();

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
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.LookAndFeelRuleImpl <em>Look And Feel Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.LookAndFeelRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getLookAndFeelRule()
		 * @generated
		 */
		EClass LOOK_AND_FEEL_RULE = eINSTANCE.getLookAndFeelRule();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LOOK_AND_FEEL_RULE__VALUE = eINSTANCE.getLookAndFeelRule_Value();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl <em>Abstract Column Descriptor</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getAbstractColumnDescriptor()
		 * @generated
		 */
		EClass ABSTRACT_COLUMN_DESCRIPTOR = eINSTANCE.getAbstractColumnDescriptor();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABSTRACT_COLUMN_DESCRIPTOR__LABEL = eINSTANCE.getAbstractColumnDescriptor_Label();

		/**
		 * The meta object literal for the '<em><b>Editing Support</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT = eINSTANCE.getAbstractColumnDescriptor_EditingSupport();

		/**
		 * The meta object literal for the '<em><b>Cell Editor</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR = eINSTANCE.getAbstractColumnDescriptor_CellEditor();

		/**
		 * The meta object literal for the '<em><b>Resizable</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE = eINSTANCE.getAbstractColumnDescriptor_Resizable();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl <em>Path Selector</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getPathSelector()
		 * @generated
		 */
		EClass PATH_SELECTOR = eINSTANCE.getPathSelector();

		/**
		 * The meta object literal for the '<em><b>EClassifier Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PATH_SELECTOR__ECLASSIFIER_NAME = eINSTANCE.getPathSelector_EClassifierName();

		/**
		 * The meta object literal for the '<em><b>Context</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PATH_SELECTOR__CONTEXT = eINSTANCE.getPathSelector_Context();

		/**
		 * The meta object literal for the '<em><b>Paths</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PATH_SELECTOR__PATHS = eINSTANCE.getPathSelector_Paths();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.DynamicProviderImpl <em>Dynamic Provider</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.DynamicProviderImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getDynamicProvider()
		 * @generated
		 */
		EClass DYNAMIC_PROVIDER = eINSTANCE.getDynamicProvider();

		/**
		 * The meta object literal for the '<em><b>Uri</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DYNAMIC_PROVIDER__URI = eINSTANCE.getDynamicProvider_Uri();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl <em>Column Descriptor</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getColumnDescriptor()
		 * @generated
		 */
		EClass COLUMN_DESCRIPTOR = eINSTANCE.getColumnDescriptor();

		/**
		 * The meta object literal for the '<em><b>Width</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLUMN_DESCRIPTOR__WIDTH = eINSTANCE.getColumnDescriptor_Width();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.WeightedColumnDescriptorImpl <em>Weighted Column Descriptor</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.WeightedColumnDescriptorImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getWeightedColumnDescriptor()
		 * @generated
		 */
		EClass WEIGHTED_COLUMN_DESCRIPTOR = eINSTANCE.getWeightedColumnDescriptor();

		/**
		 * The meta object literal for the '<em><b>Weight</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute WEIGHTED_COLUMN_DESCRIPTOR__WEIGHT = eINSTANCE.getWeightedColumnDescriptor_Weight();

		/**
		 * The meta object literal for the '<em><b>Minimum Width</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute WEIGHTED_COLUMN_DESCRIPTOR__MINIMUM_WIDTH = eINSTANCE.getWeightedColumnDescriptor_MinimumWidth();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel <em>Look And Feel</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel
		 * @see org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl#getLookAndFeel()
		 * @generated
		 */
		EEnum LOOK_AND_FEEL = eINSTANCE.getLookAndFeel();

	}

} //CoreCollectionsStylesPackage
