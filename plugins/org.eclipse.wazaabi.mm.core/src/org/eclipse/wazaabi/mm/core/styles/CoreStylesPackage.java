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
package org.eclipse.wazaabi.mm.core.styles;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

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
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory
 * @model kind="package"
 * @generated
 */
public interface CoreStylesPackage extends EPackage {
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
	String eNS_URI = "http://www.wazaabi.org/core/styles";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "wcs";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CoreStylesPackage eINSTANCE = org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.StyledElementImpl <em>Styled Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.StyledElementImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStyledElement()
	 * @generated
	 */
	int STYLED_ELEMENT = 0;

	/**
	 * The feature id for the '<em><b>Style Rules</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STYLED_ELEMENT__STYLE_RULES = 0;

	/**
	 * The number of structural features of the '<em>Styled Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STYLED_ELEMENT_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.StyleRule <em>Style Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.StyleRule
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStyleRule()
	 * @generated
	 */
	int STYLE_RULE = 1;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STYLE_RULE__PROPERTY_NAME = 0;

	/**
	 * The number of structural features of the '<em>Style Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STYLE_RULE_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl <em>Color Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getColorRule()
	 * @generated
	 */
	int COLOR_RULE = 2;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLOR_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Red</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLOR_RULE__RED = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Green</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLOR_RULE__GREEN = STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Blue</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLOR_RULE__BLUE = STYLE_RULE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Color Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLOR_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.StringRuleImpl <em>String Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.StringRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStringRule()
	 * @generated
	 */
	int STRING_RULE = 3;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STRING_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STRING_RULE__VALUE = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>String Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STRING_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.OrientationRuleImpl <em>Orientation Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.OrientationRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getOrientationRule()
	 * @generated
	 */
	int ORIENTATION_RULE = 4;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ORIENTATION_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ORIENTATION_RULE__VALUE = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Orientation Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ORIENTATION_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.BooleanRuleImpl <em>Boolean Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.BooleanRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getBooleanRule()
	 * @generated
	 */
	int BOOLEAN_RULE = 5;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BOOLEAN_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BOOLEAN_RULE__VALUE = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Boolean Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BOOLEAN_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.IntRuleImpl <em>Int Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.IntRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getIntRule()
	 * @generated
	 */
	int INT_RULE = 6;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INT_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INT_RULE__VALUE = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Int Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INT_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl <em>Blank Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getBlankRule()
	 * @generated
	 */
	int BLANK_RULE = 7;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The number of structural features of the '<em>Blank Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl <em>Font Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getFontRule()
	 * @generated
	 */
	int FONT_RULE = 8;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FONT_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FONT_RULE__NAME = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Height</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FONT_RULE__HEIGHT = STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Italic</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FONT_RULE__ITALIC = STYLE_RULE_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Bold</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FONT_RULE__BOLD = STYLE_RULE_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>Font Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FONT_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 4;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.LayoutRuleImpl <em>Layout Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.LayoutRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getLayoutRule()
	 * @generated
	 */
	int LAYOUT_RULE = 9;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAYOUT_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The number of structural features of the '<em>Layout Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAYOUT_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.StackLayoutRuleImpl <em>Stack Layout Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.StackLayoutRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStackLayoutRule()
	 * @generated
	 */
	int STACK_LAYOUT_RULE = 10;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STACK_LAYOUT_RULE__PROPERTY_NAME = LAYOUT_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Margin Height</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STACK_LAYOUT_RULE__MARGIN_HEIGHT = LAYOUT_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Margin Width</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STACK_LAYOUT_RULE__MARGIN_WIDTH = LAYOUT_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Top</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STACK_LAYOUT_RULE__TOP = LAYOUT_RULE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Stack Layout Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int STACK_LAYOUT_RULE_FEATURE_COUNT = LAYOUT_RULE_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.LayoutDataRuleImpl <em>Layout Data Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.LayoutDataRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getLayoutDataRule()
	 * @generated
	 */
	int LAYOUT_DATA_RULE = 11;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAYOUT_DATA_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The number of structural features of the '<em>Layout Data Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAYOUT_DATA_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.DirectionRuleImpl <em>Direction Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.DirectionRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getDirectionRule()
	 * @generated
	 */
	int DIRECTION_RULE = 12;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DIRECTION_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DIRECTION_RULE__VALUE = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Direction Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DIRECTION_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.MarkerImpl <em>Marker</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.MarkerImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getMarker()
	 * @generated
	 */
	int MARKER = 13;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MARKER__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The number of structural features of the '<em>Marker</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MARKER_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ImageRuleImpl <em>Image Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.ImageRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getImageRule()
	 * @generated
	 */
	int IMAGE_RULE = 14;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IMAGE_RULE__PROPERTY_NAME = STRING_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IMAGE_RULE__VALUE = STRING_RULE__VALUE;

	/**
	 * The number of structural features of the '<em>Image Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IMAGE_RULE_FEATURE_COUNT = STRING_RULE_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.TabbedLayoutRuleImpl <em>Tabbed Layout Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.TabbedLayoutRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getTabbedLayoutRule()
	 * @generated
	 */
	int TABBED_LAYOUT_RULE = 15;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE__PROPERTY_NAME = STACK_LAYOUT_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Margin Height</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE__MARGIN_HEIGHT = STACK_LAYOUT_RULE__MARGIN_HEIGHT;

	/**
	 * The feature id for the '<em><b>Margin Width</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE__MARGIN_WIDTH = STACK_LAYOUT_RULE__MARGIN_WIDTH;

	/**
	 * The feature id for the '<em><b>Top</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE__TOP = STACK_LAYOUT_RULE__TOP;

	/**
	 * The feature id for the '<em><b>Maximize Visible</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE = STACK_LAYOUT_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Minimize Visible</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE = STACK_LAYOUT_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Position</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE__POSITION = STACK_LAYOUT_RULE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Tabbed Layout Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TABBED_LAYOUT_RULE_FEATURE_COUNT = STACK_LAYOUT_RULE_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.TabRuleImpl <em>Tab Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.TabRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getTabRule()
	 * @generated
	 */
	int TAB_RULE = 16;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAB_RULE__PROPERTY_NAME = LAYOUT_DATA_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Label</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAB_RULE__LABEL = LAYOUT_DATA_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Image</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAB_RULE__IMAGE = LAYOUT_DATA_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Closable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAB_RULE__CLOSABLE = LAYOUT_DATA_RULE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Tab Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAB_RULE_FEATURE_COUNT = LAYOUT_DATA_RULE_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.BarLayoutRuleImpl <em>Bar Layout Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.BarLayoutRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getBarLayoutRule()
	 * @generated
	 */
	int BAR_LAYOUT_RULE = 17;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BAR_LAYOUT_RULE__PROPERTY_NAME = LAYOUT_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Draggable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BAR_LAYOUT_RULE__DRAGGABLE = LAYOUT_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Bar Layout Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BAR_LAYOUT_RULE_FEATURE_COUNT = LAYOUT_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ExpandRuleImpl <em>Expand Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.ExpandRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getExpandRule()
	 * @generated
	 */
	int EXPAND_RULE = 18;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXPAND_RULE__PROPERTY_NAME = LAYOUT_DATA_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Label</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXPAND_RULE__LABEL = LAYOUT_DATA_RULE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Expanded</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXPAND_RULE__EXPANDED = LAYOUT_DATA_RULE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Image</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXPAND_RULE__IMAGE = LAYOUT_DATA_RULE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Expand Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXPAND_RULE_FEATURE_COUNT = LAYOUT_DATA_RULE_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ExpandLayoutRuleImpl <em>Expand Layout Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.ExpandLayoutRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getExpandLayoutRule()
	 * @generated
	 */
	int EXPAND_LAYOUT_RULE = 19;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXPAND_LAYOUT_RULE__PROPERTY_NAME = LAYOUT_RULE__PROPERTY_NAME;

	/**
	 * The number of structural features of the '<em>Expand Layout Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXPAND_LAYOUT_RULE_FEATURE_COUNT = LAYOUT_RULE_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.SashFormLayoutRuleImpl <em>Sash Form Layout Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.SashFormLayoutRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getSashFormLayoutRule()
	 * @generated
	 */
	int SASH_FORM_LAYOUT_RULE = 20;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SASH_FORM_LAYOUT_RULE__PROPERTY_NAME = LAYOUT_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Orientation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SASH_FORM_LAYOUT_RULE__ORIENTATION = LAYOUT_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Sash Form Layout Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SASH_FORM_LAYOUT_RULE_FEATURE_COUNT = LAYOUT_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.HyperlinkRuleImpl <em>Hyperlink Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.HyperlinkRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getHyperlinkRule()
	 * @generated
	 */
	int HYPERLINK_RULE = 21;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int HYPERLINK_RULE__PROPERTY_NAME = LAYOUT_RULE__PROPERTY_NAME;

	/**
	 * The number of structural features of the '<em>Hyperlink Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int HYPERLINK_RULE_FEATURE_COUNT = LAYOUT_RULE_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.SashRuleImpl <em>Sash Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.SashRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getSashRule()
	 * @generated
	 */
	int SASH_RULE = 22;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SASH_RULE__PROPERTY_NAME = LAYOUT_DATA_RULE__PROPERTY_NAME;

	/**
	 * The feature id for the '<em><b>Weight</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SASH_RULE__WEIGHT = LAYOUT_DATA_RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Sash Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SASH_RULE_FEATURE_COUNT = LAYOUT_DATA_RULE_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ScrollBarRuleImpl <em>Scroll Bar Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.ScrollBarRuleImpl
	 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getScrollBarRule()
	 * @generated
	 */
	int SCROLL_BAR_RULE = 23;

	/**
	 * The feature id for the '<em><b>Property Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SCROLL_BAR_RULE__PROPERTY_NAME = STYLE_RULE__PROPERTY_NAME;

	/**
	 * The number of structural features of the '<em>Scroll Bar Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SCROLL_BAR_RULE_FEATURE_COUNT = STYLE_RULE_FEATURE_COUNT + 0;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.StyledElement <em>Styled Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Styled Element</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StyledElement
	 * @generated
	 */
	EClass getStyledElement();

	/**
	 * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.core.styles.StyledElement#getStyleRules <em>Style Rules</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Style Rules</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StyledElement#getStyleRules()
	 * @see #getStyledElement()
	 * @generated
	 */
	EReference getStyledElement_StyleRules();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.StyleRule <em>Style Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Style Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StyleRule
	 * @generated
	 */
	EClass getStyleRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.StyleRule#getPropertyName <em>Property Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Property Name</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StyleRule#getPropertyName()
	 * @see #getStyleRule()
	 * @generated
	 */
	EAttribute getStyleRule_PropertyName();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule <em>Color Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Color Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ColorRule
	 * @generated
	 */
	EClass getColorRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getRed <em>Red</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Red</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ColorRule#getRed()
	 * @see #getColorRule()
	 * @generated
	 */
	EAttribute getColorRule_Red();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getGreen <em>Green</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Green</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ColorRule#getGreen()
	 * @see #getColorRule()
	 * @generated
	 */
	EAttribute getColorRule_Green();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getBlue <em>Blue</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Blue</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ColorRule#getBlue()
	 * @see #getColorRule()
	 * @generated
	 */
	EAttribute getColorRule_Blue();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.StringRule <em>String Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>String Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StringRule
	 * @generated
	 */
	EClass getStringRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.StringRule#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StringRule#getValue()
	 * @see #getStringRule()
	 * @generated
	 */
	EAttribute getStringRule_Value();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.OrientationRule <em>Orientation Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Orientation Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.OrientationRule
	 * @generated
	 */
	EClass getOrientationRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.OrientationRule#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.OrientationRule#getValue()
	 * @see #getOrientationRule()
	 * @generated
	 */
	EAttribute getOrientationRule_Value();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.BooleanRule <em>Boolean Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Boolean Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.BooleanRule
	 * @generated
	 */
	EClass getBooleanRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.BooleanRule#isValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.BooleanRule#isValue()
	 * @see #getBooleanRule()
	 * @generated
	 */
	EAttribute getBooleanRule_Value();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.IntRule <em>Int Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Int Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.IntRule
	 * @generated
	 */
	EClass getIntRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.IntRule#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.IntRule#getValue()
	 * @see #getIntRule()
	 * @generated
	 */
	EAttribute getIntRule_Value();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.BlankRule <em>Blank Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Blank Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.BlankRule
	 * @generated
	 */
	EClass getBlankRule();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.FontRule <em>Font Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Font Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.FontRule
	 * @generated
	 */
	EClass getFontRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.FontRule#getName()
	 * @see #getFontRule()
	 * @generated
	 */
	EAttribute getFontRule_Name();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getHeight <em>Height</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Height</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.FontRule#getHeight()
	 * @see #getFontRule()
	 * @generated
	 */
	EAttribute getFontRule_Height();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isItalic <em>Italic</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Italic</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.FontRule#isItalic()
	 * @see #getFontRule()
	 * @generated
	 */
	EAttribute getFontRule_Italic();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isBold <em>Bold</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Bold</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.FontRule#isBold()
	 * @see #getFontRule()
	 * @generated
	 */
	EAttribute getFontRule_Bold();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.LayoutRule <em>Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Layout Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.LayoutRule
	 * @generated
	 */
	EClass getLayoutRule();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule <em>Stack Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Stack Layout Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StackLayoutRule
	 * @generated
	 */
	EClass getStackLayoutRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginHeight <em>Margin Height</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Margin Height</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginHeight()
	 * @see #getStackLayoutRule()
	 * @generated
	 */
	EAttribute getStackLayoutRule_MarginHeight();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginWidth <em>Margin Width</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Margin Width</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginWidth()
	 * @see #getStackLayoutRule()
	 * @generated
	 */
	EAttribute getStackLayoutRule_MarginWidth();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getTop <em>Top</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Top</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getTop()
	 * @see #getStackLayoutRule()
	 * @generated
	 */
	EAttribute getStackLayoutRule_Top();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.LayoutDataRule <em>Layout Data Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Layout Data Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.LayoutDataRule
	 * @generated
	 */
	EClass getLayoutDataRule();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.DirectionRule <em>Direction Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Direction Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.DirectionRule
	 * @generated
	 */
	EClass getDirectionRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.DirectionRule#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.DirectionRule#getValue()
	 * @see #getDirectionRule()
	 * @generated
	 */
	EAttribute getDirectionRule_Value();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.Marker <em>Marker</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Marker</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.Marker
	 * @generated
	 */
	EClass getMarker();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.ImageRule <em>Image Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Image Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ImageRule
	 * @generated
	 */
	EClass getImageRule();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule <em>Tabbed Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Tabbed Layout Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule
	 * @generated
	 */
	EClass getTabbedLayoutRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMaximizeVisible <em>Maximize Visible</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Maximize Visible</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMaximizeVisible()
	 * @see #getTabbedLayoutRule()
	 * @generated
	 */
	EAttribute getTabbedLayoutRule_MaximizeVisible();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMinimizeVisible <em>Minimize Visible</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Minimize Visible</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMinimizeVisible()
	 * @see #getTabbedLayoutRule()
	 * @generated
	 */
	EAttribute getTabbedLayoutRule_MinimizeVisible();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#getPosition <em>Position</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Position</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#getPosition()
	 * @see #getTabbedLayoutRule()
	 * @generated
	 */
	EAttribute getTabbedLayoutRule_Position();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.TabRule <em>Tab Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Tab Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabRule
	 * @generated
	 */
	EClass getTabRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#getLabel <em>Label</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Label</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabRule#getLabel()
	 * @see #getTabRule()
	 * @generated
	 */
	EAttribute getTabRule_Label();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#getImage <em>Image</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Image</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabRule#getImage()
	 * @see #getTabRule()
	 * @generated
	 */
	EAttribute getTabRule_Image();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#isClosable <em>Closable</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Closable</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabRule#isClosable()
	 * @see #getTabRule()
	 * @generated
	 */
	EAttribute getTabRule_Closable();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.BarLayoutRule <em>Bar Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Bar Layout Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.BarLayoutRule
	 * @generated
	 */
	EClass getBarLayoutRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.BarLayoutRule#isDraggable <em>Draggable</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Draggable</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.BarLayoutRule#isDraggable()
	 * @see #getBarLayoutRule()
	 * @generated
	 */
	EAttribute getBarLayoutRule_Draggable();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule <em>Expand Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Expand Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ExpandRule
	 * @generated
	 */
	EClass getExpandRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#getLabel <em>Label</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Label</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ExpandRule#getLabel()
	 * @see #getExpandRule()
	 * @generated
	 */
	EAttribute getExpandRule_Label();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#isExpanded <em>Expanded</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Expanded</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ExpandRule#isExpanded()
	 * @see #getExpandRule()
	 * @generated
	 */
	EAttribute getExpandRule_Expanded();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#getImage <em>Image</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Image</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ExpandRule#getImage()
	 * @see #getExpandRule()
	 * @generated
	 */
	EAttribute getExpandRule_Image();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule <em>Expand Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Expand Layout Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule
	 * @generated
	 */
	EClass getExpandLayoutRule();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule <em>Sash Form Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Sash Form Layout Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule
	 * @generated
	 */
	EClass getSashFormLayoutRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule#getOrientation <em>Orientation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Orientation</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule#getOrientation()
	 * @see #getSashFormLayoutRule()
	 * @generated
	 */
	EAttribute getSashFormLayoutRule_Orientation();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.HyperlinkRule <em>Hyperlink Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Hyperlink Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.HyperlinkRule
	 * @generated
	 */
	EClass getHyperlinkRule();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.SashRule <em>Sash Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Sash Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.SashRule
	 * @generated
	 */
	EClass getSashRule();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.styles.SashRule#getWeight <em>Weight</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Weight</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.SashRule#getWeight()
	 * @see #getSashRule()
	 * @generated
	 */
	EAttribute getSashRule_Weight();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.styles.ScrollBarRule <em>Scroll Bar Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Scroll Bar Rule</em>'.
	 * @see org.eclipse.wazaabi.mm.core.styles.ScrollBarRule
	 * @generated
	 */
	EClass getScrollBarRule();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CoreStylesFactory getCoreStylesFactory();

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
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.StyledElementImpl <em>Styled Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.StyledElementImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStyledElement()
		 * @generated
		 */
		EClass STYLED_ELEMENT = eINSTANCE.getStyledElement();

		/**
		 * The meta object literal for the '<em><b>Style Rules</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference STYLED_ELEMENT__STYLE_RULES = eINSTANCE.getStyledElement_StyleRules();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.StyleRule <em>Style Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.StyleRule
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStyleRule()
		 * @generated
		 */
		EClass STYLE_RULE = eINSTANCE.getStyleRule();

		/**
		 * The meta object literal for the '<em><b>Property Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute STYLE_RULE__PROPERTY_NAME = eINSTANCE.getStyleRule_PropertyName();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl <em>Color Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getColorRule()
		 * @generated
		 */
		EClass COLOR_RULE = eINSTANCE.getColorRule();

		/**
		 * The meta object literal for the '<em><b>Red</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLOR_RULE__RED = eINSTANCE.getColorRule_Red();

		/**
		 * The meta object literal for the '<em><b>Green</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLOR_RULE__GREEN = eINSTANCE.getColorRule_Green();

		/**
		 * The meta object literal for the '<em><b>Blue</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLOR_RULE__BLUE = eINSTANCE.getColorRule_Blue();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.StringRuleImpl <em>String Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.StringRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStringRule()
		 * @generated
		 */
		EClass STRING_RULE = eINSTANCE.getStringRule();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute STRING_RULE__VALUE = eINSTANCE.getStringRule_Value();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.OrientationRuleImpl <em>Orientation Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.OrientationRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getOrientationRule()
		 * @generated
		 */
		EClass ORIENTATION_RULE = eINSTANCE.getOrientationRule();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ORIENTATION_RULE__VALUE = eINSTANCE.getOrientationRule_Value();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.BooleanRuleImpl <em>Boolean Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.BooleanRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getBooleanRule()
		 * @generated
		 */
		EClass BOOLEAN_RULE = eINSTANCE.getBooleanRule();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BOOLEAN_RULE__VALUE = eINSTANCE.getBooleanRule_Value();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.IntRuleImpl <em>Int Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.IntRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getIntRule()
		 * @generated
		 */
		EClass INT_RULE = eINSTANCE.getIntRule();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute INT_RULE__VALUE = eINSTANCE.getIntRule_Value();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl <em>Blank Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getBlankRule()
		 * @generated
		 */
		EClass BLANK_RULE = eINSTANCE.getBlankRule();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl <em>Font Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getFontRule()
		 * @generated
		 */
		EClass FONT_RULE = eINSTANCE.getFontRule();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FONT_RULE__NAME = eINSTANCE.getFontRule_Name();

		/**
		 * The meta object literal for the '<em><b>Height</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FONT_RULE__HEIGHT = eINSTANCE.getFontRule_Height();

		/**
		 * The meta object literal for the '<em><b>Italic</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FONT_RULE__ITALIC = eINSTANCE.getFontRule_Italic();

		/**
		 * The meta object literal for the '<em><b>Bold</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FONT_RULE__BOLD = eINSTANCE.getFontRule_Bold();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.LayoutRuleImpl <em>Layout Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.LayoutRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getLayoutRule()
		 * @generated
		 */
		EClass LAYOUT_RULE = eINSTANCE.getLayoutRule();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.StackLayoutRuleImpl <em>Stack Layout Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.StackLayoutRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getStackLayoutRule()
		 * @generated
		 */
		EClass STACK_LAYOUT_RULE = eINSTANCE.getStackLayoutRule();

		/**
		 * The meta object literal for the '<em><b>Margin Height</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute STACK_LAYOUT_RULE__MARGIN_HEIGHT = eINSTANCE.getStackLayoutRule_MarginHeight();

		/**
		 * The meta object literal for the '<em><b>Margin Width</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute STACK_LAYOUT_RULE__MARGIN_WIDTH = eINSTANCE.getStackLayoutRule_MarginWidth();

		/**
		 * The meta object literal for the '<em><b>Top</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute STACK_LAYOUT_RULE__TOP = eINSTANCE.getStackLayoutRule_Top();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.LayoutDataRuleImpl <em>Layout Data Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.LayoutDataRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getLayoutDataRule()
		 * @generated
		 */
		EClass LAYOUT_DATA_RULE = eINSTANCE.getLayoutDataRule();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.DirectionRuleImpl <em>Direction Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.DirectionRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getDirectionRule()
		 * @generated
		 */
		EClass DIRECTION_RULE = eINSTANCE.getDirectionRule();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DIRECTION_RULE__VALUE = eINSTANCE.getDirectionRule_Value();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.MarkerImpl <em>Marker</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.MarkerImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getMarker()
		 * @generated
		 */
		EClass MARKER = eINSTANCE.getMarker();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ImageRuleImpl <em>Image Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.ImageRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getImageRule()
		 * @generated
		 */
		EClass IMAGE_RULE = eINSTANCE.getImageRule();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.TabbedLayoutRuleImpl <em>Tabbed Layout Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.TabbedLayoutRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getTabbedLayoutRule()
		 * @generated
		 */
		EClass TABBED_LAYOUT_RULE = eINSTANCE.getTabbedLayoutRule();

		/**
		 * The meta object literal for the '<em><b>Maximize Visible</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE = eINSTANCE.getTabbedLayoutRule_MaximizeVisible();

		/**
		 * The meta object literal for the '<em><b>Minimize Visible</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE = eINSTANCE.getTabbedLayoutRule_MinimizeVisible();

		/**
		 * The meta object literal for the '<em><b>Position</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TABBED_LAYOUT_RULE__POSITION = eINSTANCE.getTabbedLayoutRule_Position();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.TabRuleImpl <em>Tab Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.TabRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getTabRule()
		 * @generated
		 */
		EClass TAB_RULE = eINSTANCE.getTabRule();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TAB_RULE__LABEL = eINSTANCE.getTabRule_Label();

		/**
		 * The meta object literal for the '<em><b>Image</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TAB_RULE__IMAGE = eINSTANCE.getTabRule_Image();

		/**
		 * The meta object literal for the '<em><b>Closable</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TAB_RULE__CLOSABLE = eINSTANCE.getTabRule_Closable();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.BarLayoutRuleImpl <em>Bar Layout Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.BarLayoutRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getBarLayoutRule()
		 * @generated
		 */
		EClass BAR_LAYOUT_RULE = eINSTANCE.getBarLayoutRule();

		/**
		 * The meta object literal for the '<em><b>Draggable</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BAR_LAYOUT_RULE__DRAGGABLE = eINSTANCE.getBarLayoutRule_Draggable();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ExpandRuleImpl <em>Expand Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.ExpandRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getExpandRule()
		 * @generated
		 */
		EClass EXPAND_RULE = eINSTANCE.getExpandRule();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EXPAND_RULE__LABEL = eINSTANCE.getExpandRule_Label();

		/**
		 * The meta object literal for the '<em><b>Expanded</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EXPAND_RULE__EXPANDED = eINSTANCE.getExpandRule_Expanded();

		/**
		 * The meta object literal for the '<em><b>Image</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EXPAND_RULE__IMAGE = eINSTANCE.getExpandRule_Image();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ExpandLayoutRuleImpl <em>Expand Layout Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.ExpandLayoutRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getExpandLayoutRule()
		 * @generated
		 */
		EClass EXPAND_LAYOUT_RULE = eINSTANCE.getExpandLayoutRule();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.SashFormLayoutRuleImpl <em>Sash Form Layout Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.SashFormLayoutRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getSashFormLayoutRule()
		 * @generated
		 */
		EClass SASH_FORM_LAYOUT_RULE = eINSTANCE.getSashFormLayoutRule();

		/**
		 * The meta object literal for the '<em><b>Orientation</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SASH_FORM_LAYOUT_RULE__ORIENTATION = eINSTANCE.getSashFormLayoutRule_Orientation();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.HyperlinkRuleImpl <em>Hyperlink Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.HyperlinkRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getHyperlinkRule()
		 * @generated
		 */
		EClass HYPERLINK_RULE = eINSTANCE.getHyperlinkRule();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.SashRuleImpl <em>Sash Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.SashRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getSashRule()
		 * @generated
		 */
		EClass SASH_RULE = eINSTANCE.getSashRule();

		/**
		 * The meta object literal for the '<em><b>Weight</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SASH_RULE__WEIGHT = eINSTANCE.getSashRule_Weight();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.styles.impl.ScrollBarRuleImpl <em>Scroll Bar Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.ScrollBarRuleImpl
		 * @see org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesPackageImpl#getScrollBarRule()
		 * @generated
		 */
		EClass SCROLL_BAR_RULE = eINSTANCE.getScrollBarRule();

	}

} //CoreStylesPackage
