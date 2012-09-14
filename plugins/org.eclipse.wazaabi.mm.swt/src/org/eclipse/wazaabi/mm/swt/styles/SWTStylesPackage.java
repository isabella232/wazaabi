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
package org.eclipse.wazaabi.mm.swt.styles;

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
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory
 * @model kind="package"
 * @generated
 */
public interface SWTStylesPackage extends EPackage
{
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
  String eNS_URI = "http://www.wazaabi.org/swt/styles";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "swtstyles";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  SWTStylesPackage eINSTANCE = org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl.init();

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.RowLayoutRuleImpl <em>Row Layout Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.RowLayoutRuleImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getRowLayoutRule()
   * @generated
   */
  int ROW_LAYOUT_RULE = 0;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

  /**
   * The feature id for the '<em><b>Center</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__CENTER = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Fill</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__FILL = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Justify</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__JUSTIFY = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Margin Bottom</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__MARGIN_BOTTOM = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Margin Height</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__MARGIN_HEIGHT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Margin Left</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__MARGIN_LEFT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 5;

  /**
   * The feature id for the '<em><b>Margin Right</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__MARGIN_RIGHT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 6;

  /**
   * The feature id for the '<em><b>Margin Top</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__MARGIN_TOP = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 7;

  /**
   * The feature id for the '<em><b>Margin Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__MARGIN_WIDTH = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 8;

  /**
   * The feature id for the '<em><b>Pack</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__PACK = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 9;

  /**
   * The feature id for the '<em><b>Spacing</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__SPACING = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 10;

  /**
   * The feature id for the '<em><b>Type</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__TYPE = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 11;

  /**
   * The feature id for the '<em><b>Wrap</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE__WRAP = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 12;

  /**
   * The number of structural features of the '<em>Row Layout Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_LAYOUT_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 13;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.RowDataRuleImpl <em>Row Data Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.RowDataRuleImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getRowDataRule()
   * @generated
   */
  int ROW_DATA_RULE = 1;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_DATA_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_DATA_RULE__PROPERTY_NAME;

  /**
   * The feature id for the '<em><b>Exclude</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_DATA_RULE__EXCLUDE = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_DATA_RULE__WIDTH = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Height</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_DATA_RULE__HEIGHT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 2;

  /**
   * The number of structural features of the '<em>Row Data Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ROW_DATA_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 3;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridLayoutRuleImpl <em>Grid Layout Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.GridLayoutRuleImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getGridLayoutRule()
   * @generated
   */
  int GRID_LAYOUT_RULE = 2;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

  /**
   * The feature id for the '<em><b>Horizontal Spacing</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__HORIZONTAL_SPACING = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Make Columns Equal Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__MAKE_COLUMNS_EQUAL_WIDTH = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Margin Bottom</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__MARGIN_BOTTOM = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Margin Height</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__MARGIN_HEIGHT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Margin Left</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__MARGIN_LEFT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Margin Right</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__MARGIN_RIGHT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 5;

  /**
   * The feature id for the '<em><b>Margin Top</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__MARGIN_TOP = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 6;

  /**
   * The feature id for the '<em><b>Margin Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__MARGIN_WIDTH = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 7;

  /**
   * The feature id for the '<em><b>Num Columns</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__NUM_COLUMNS = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 8;

  /**
   * The feature id for the '<em><b>Vertical Spacing</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE__VERTICAL_SPACING = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 9;

  /**
   * The number of structural features of the '<em>Grid Layout Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_LAYOUT_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 10;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl <em>Grid Data Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getGridDataRule()
   * @generated
   */
  int GRID_DATA_RULE = 3;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_DATA_RULE__PROPERTY_NAME;

  /**
   * The feature id for the '<em><b>Exclude</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__EXCLUDE = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Grab Excess Horizontal Space</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Grab Excess Vertical Space</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Height Hint</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__HEIGHT_HINT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Horizontal Alignement</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Horizontal Indent</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__HORIZONTAL_INDENT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 5;

  /**
   * The feature id for the '<em><b>Horizontal Span</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__HORIZONTAL_SPAN = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 6;

  /**
   * The feature id for the '<em><b>Minimum Height</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__MINIMUM_HEIGHT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 7;

  /**
   * The feature id for the '<em><b>Minimum Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__MINIMUM_WIDTH = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 8;

  /**
   * The feature id for the '<em><b>Vertical Alignement</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__VERTICAL_ALIGNEMENT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 9;

  /**
   * The feature id for the '<em><b>Vertical Indent</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__VERTICAL_INDENT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 10;

  /**
   * The feature id for the '<em><b>Vertical Span</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__VERTICAL_SPAN = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 11;

  /**
   * The feature id for the '<em><b>Width Hint</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE__WIDTH_HINT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 12;

  /**
   * The number of structural features of the '<em>Grid Data Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int GRID_DATA_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 13;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl <em>Fill Layout Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFillLayoutRule()
   * @generated
   */
  int FILL_LAYOUT_RULE = 4;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FILL_LAYOUT_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

  /**
   * The feature id for the '<em><b>Margin Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FILL_LAYOUT_RULE__MARGIN_WIDTH = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Margin Height</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FILL_LAYOUT_RULE__MARGIN_HEIGHT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Spacing</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FILL_LAYOUT_RULE__SPACING = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Type</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FILL_LAYOUT_RULE__TYPE = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 3;

  /**
   * The number of structural features of the '<em>Fill Layout Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FILL_LAYOUT_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 4;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.FormAttachment <em>Form Attachment</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.FormAttachment
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFormAttachment()
   * @generated
   */
  int FORM_ATTACHMENT = 5;

  /**
   * The feature id for the '<em><b>Offset</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_ATTACHMENT__OFFSET = 0;

  /**
   * The number of structural features of the '<em>Form Attachment</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_ATTACHMENT_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToSiblingImpl <em>Attachment To Sibling</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToSiblingImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getAttachmentToSibling()
   * @generated
   */
  int ATTACHMENT_TO_SIBLING = 6;

  /**
   * The feature id for the '<em><b>Offset</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_SIBLING__OFFSET = FORM_ATTACHMENT__OFFSET;

  /**
   * The feature id for the '<em><b>Sibling Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_SIBLING__SIBLING_ID = FORM_ATTACHMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Alignment</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_SIBLING__ALIGNMENT = FORM_ATTACHMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Attachment To Sibling</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_SIBLING_FEATURE_COUNT = FORM_ATTACHMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToContainerImpl <em>Attachment To Container</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToContainerImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getAttachmentToContainer()
   * @generated
   */
  int ATTACHMENT_TO_CONTAINER = 7;

  /**
   * The feature id for the '<em><b>Offset</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_CONTAINER__OFFSET = FORM_ATTACHMENT__OFFSET;

  /**
   * The feature id for the '<em><b>Numerator</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_CONTAINER__NUMERATOR = FORM_ATTACHMENT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Denominator</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_CONTAINER__DENOMINATOR = FORM_ATTACHMENT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Attachment To Container</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ATTACHMENT_TO_CONTAINER_FEATURE_COUNT = FORM_ATTACHMENT_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.FormDataRuleImpl <em>Form Data Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.FormDataRuleImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFormDataRule()
   * @generated
   */
  int FORM_DATA_RULE = 8;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_DATA_RULE__PROPERTY_NAME;

  /**
   * The feature id for the '<em><b>Bottom</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE__BOTTOM = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Left</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE__LEFT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Right</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE__RIGHT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Top</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE__TOP = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Height</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE__HEIGHT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE__WIDTH = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 5;

  /**
   * The number of structural features of the '<em>Form Data Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_DATA_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_DATA_RULE_FEATURE_COUNT + 6;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.FormLayoutRuleImpl <em>Form Layout Rule</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.FormLayoutRuleImpl
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFormLayoutRule()
   * @generated
   */
  int FORM_LAYOUT_RULE = 9;

  /**
   * The feature id for the '<em><b>Property Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__PROPERTY_NAME = CoreStylesPackage.LAYOUT_RULE__PROPERTY_NAME;

  /**
   * The feature id for the '<em><b>Margin Bottom</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__MARGIN_BOTTOM = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Margin Height</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__MARGIN_HEIGHT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Margin Left</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__MARGIN_LEFT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Margin Right</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__MARGIN_RIGHT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Margin Top</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__MARGIN_TOP = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Margin Width</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__MARGIN_WIDTH = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 5;

  /**
   * The feature id for the '<em><b>Spacing</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE__SPACING = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 6;

  /**
   * The number of structural features of the '<em>Form Layout Rule</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int FORM_LAYOUT_RULE_FEATURE_COUNT = CoreStylesPackage.LAYOUT_RULE_FEATURE_COUNT + 7;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment <em>Grid Data Alignment</em>}' enum.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getGridDataAlignment()
   * @generated
   */
  int GRID_DATA_ALIGNMENT = 10;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment <em>To Sibling Alignment</em>}' enum.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment
   * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getToSiblingAlignment()
   * @generated
   */
  int TO_SIBLING_ALIGNMENT = 11;


  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule <em>Row Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Row Layout Rule</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule
   * @generated
   */
  EClass getRowLayoutRule();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isCenter <em>Center</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Center</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isCenter()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_Center();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isFill <em>Fill</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Fill</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isFill()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_Fill();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isJustify <em>Justify</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Justify</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isJustify()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_Justify();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginBottom <em>Margin Bottom</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Bottom</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginBottom()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_MarginBottom();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginHeight <em>Margin Height</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Height</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginHeight()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_MarginHeight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginLeft <em>Margin Left</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Left</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginLeft()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_MarginLeft();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginRight <em>Margin Right</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Right</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginRight()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_MarginRight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginTop <em>Margin Top</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Top</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginTop()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_MarginTop();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginWidth <em>Margin Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getMarginWidth()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_MarginWidth();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isPack <em>Pack</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Pack</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isPack()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_Pack();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getSpacing <em>Spacing</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Spacing</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getSpacing()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_Spacing();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getType <em>Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Type</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#getType()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_Type();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isWrap <em>Wrap</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Wrap</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule#isWrap()
   * @see #getRowLayoutRule()
   * @generated
   */
  EAttribute getRowLayoutRule_Wrap();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule <em>Row Data Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Row Data Rule</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowDataRule
   * @generated
   */
  EClass getRowDataRule();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#isExclude <em>Exclude</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Exclude</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowDataRule#isExclude()
   * @see #getRowDataRule()
   * @generated
   */
  EAttribute getRowDataRule_Exclude();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getWidth <em>Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getWidth()
   * @see #getRowDataRule()
   * @generated
   */
  EAttribute getRowDataRule_Width();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getHeight <em>Height</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Height</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getHeight()
   * @see #getRowDataRule()
   * @generated
   */
  EAttribute getRowDataRule_Height();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule <em>Grid Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Grid Layout Rule</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule
   * @generated
   */
  EClass getGridLayoutRule();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getHorizontalSpacing <em>Horizontal Spacing</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Horizontal Spacing</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getHorizontalSpacing()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_HorizontalSpacing();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#isMakeColumnsEqualWidth <em>Make Columns Equal Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Make Columns Equal Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#isMakeColumnsEqualWidth()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_MakeColumnsEqualWidth();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginBottom <em>Margin Bottom</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Bottom</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginBottom()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_MarginBottom();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginHeight <em>Margin Height</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Height</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginHeight()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_MarginHeight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginLeft <em>Margin Left</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Left</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginLeft()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_MarginLeft();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginRight <em>Margin Right</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Right</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginRight()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_MarginRight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginTop <em>Margin Top</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Top</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginTop()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_MarginTop();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginWidth <em>Margin Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getMarginWidth()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_MarginWidth();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getNumColumns <em>Num Columns</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Num Columns</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getNumColumns()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_NumColumns();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getVerticalSpacing <em>Vertical Spacing</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Vertical Spacing</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule#getVerticalSpacing()
   * @see #getGridLayoutRule()
   * @generated
   */
  EAttribute getGridLayoutRule_VerticalSpacing();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule <em>Grid Data Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Grid Data Rule</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule
   * @generated
   */
  EClass getGridDataRule();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isExclude <em>Exclude</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Exclude</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isExclude()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_Exclude();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessHorizontalSpace <em>Grab Excess Horizontal Space</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Grab Excess Horizontal Space</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessHorizontalSpace()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_GrabExcessHorizontalSpace();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessVerticalSpace <em>Grab Excess Vertical Space</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Grab Excess Vertical Space</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#isGrabExcessVerticalSpace()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_GrabExcessVerticalSpace();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHeightHint <em>Height Hint</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Height Hint</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHeightHint()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_HeightHint();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalAlignement <em>Horizontal Alignement</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Horizontal Alignement</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalAlignement()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_HorizontalAlignement();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalIndent <em>Horizontal Indent</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Horizontal Indent</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalIndent()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_HorizontalIndent();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalSpan <em>Horizontal Span</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Horizontal Span</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getHorizontalSpan()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_HorizontalSpan();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumHeight <em>Minimum Height</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Minimum Height</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumHeight()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_MinimumHeight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumWidth <em>Minimum Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Minimum Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getMinimumWidth()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_MinimumWidth();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalAlignement <em>Vertical Alignement</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Vertical Alignement</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalAlignement()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_VerticalAlignement();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalIndent <em>Vertical Indent</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Vertical Indent</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalIndent()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_VerticalIndent();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalSpan <em>Vertical Span</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Vertical Span</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getVerticalSpan()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_VerticalSpan();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getWidthHint <em>Width Hint</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Width Hint</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule#getWidthHint()
   * @see #getGridDataRule()
   * @generated
   */
  EAttribute getGridDataRule_WidthHint();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule <em>Fill Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Fill Layout Rule</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule
   * @generated
   */
  EClass getFillLayoutRule();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginWidth <em>Margin Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginWidth()
   * @see #getFillLayoutRule()
   * @generated
   */
  EAttribute getFillLayoutRule_MarginWidth();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginHeight <em>Margin Height</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Height</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginHeight()
   * @see #getFillLayoutRule()
   * @generated
   */
  EAttribute getFillLayoutRule_MarginHeight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getSpacing <em>Spacing</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Spacing</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getSpacing()
   * @see #getFillLayoutRule()
   * @generated
   */
  EAttribute getFillLayoutRule_Spacing();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getType <em>Type</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Type</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getType()
   * @see #getFillLayoutRule()
   * @generated
   */
  EAttribute getFillLayoutRule_Type();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.FormAttachment <em>Form Attachment</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Form Attachment</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormAttachment
   * @generated
   */
  EClass getFormAttachment();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormAttachment#getOffset <em>Offset</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Offset</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormAttachment#getOffset()
   * @see #getFormAttachment()
   * @generated
   */
  EAttribute getFormAttachment_Offset();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling <em>Attachment To Sibling</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Attachment To Sibling</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling
   * @generated
   */
  EClass getAttachmentToSibling();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getSiblingId <em>Sibling Id</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Sibling Id</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getSiblingId()
   * @see #getAttachmentToSibling()
   * @generated
   */
  EAttribute getAttachmentToSibling_SiblingId();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getAlignment <em>Alignment</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Alignment</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getAlignment()
   * @see #getAttachmentToSibling()
   * @generated
   */
  EAttribute getAttachmentToSibling_Alignment();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer <em>Attachment To Container</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Attachment To Container</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer
   * @generated
   */
  EClass getAttachmentToContainer();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getNumerator <em>Numerator</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Numerator</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getNumerator()
   * @see #getAttachmentToContainer()
   * @generated
   */
  EAttribute getAttachmentToContainer_Numerator();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getDenominator <em>Denominator</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Denominator</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getDenominator()
   * @see #getAttachmentToContainer()
   * @generated
   */
  EAttribute getAttachmentToContainer_Denominator();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule <em>Form Data Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Form Data Rule</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule
   * @generated
   */
  EClass getFormDataRule();

  /**
   * Returns the meta object for the containment reference '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getBottom <em>Bottom</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Bottom</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getBottom()
   * @see #getFormDataRule()
   * @generated
   */
  EReference getFormDataRule_Bottom();

  /**
   * Returns the meta object for the containment reference '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getLeft <em>Left</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Left</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getLeft()
   * @see #getFormDataRule()
   * @generated
   */
  EReference getFormDataRule_Left();

  /**
   * Returns the meta object for the containment reference '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getRight <em>Right</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Right</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getRight()
   * @see #getFormDataRule()
   * @generated
   */
  EReference getFormDataRule_Right();

  /**
   * Returns the meta object for the containment reference '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getTop <em>Top</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference '<em>Top</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getTop()
   * @see #getFormDataRule()
   * @generated
   */
  EReference getFormDataRule_Top();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getHeight <em>Height</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Height</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getHeight()
   * @see #getFormDataRule()
   * @generated
   */
  EAttribute getFormDataRule_Height();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getWidth <em>Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getWidth()
   * @see #getFormDataRule()
   * @generated
   */
  EAttribute getFormDataRule_Width();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule <em>Form Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Form Layout Rule</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule
   * @generated
   */
  EClass getFormLayoutRule();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginBottom <em>Margin Bottom</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Bottom</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginBottom()
   * @see #getFormLayoutRule()
   * @generated
   */
  EAttribute getFormLayoutRule_MarginBottom();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginHeight <em>Margin Height</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Height</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginHeight()
   * @see #getFormLayoutRule()
   * @generated
   */
  EAttribute getFormLayoutRule_MarginHeight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginLeft <em>Margin Left</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Left</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginLeft()
   * @see #getFormLayoutRule()
   * @generated
   */
  EAttribute getFormLayoutRule_MarginLeft();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginRight <em>Margin Right</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Right</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginRight()
   * @see #getFormLayoutRule()
   * @generated
   */
  EAttribute getFormLayoutRule_MarginRight();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginTop <em>Margin Top</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Top</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginTop()
   * @see #getFormLayoutRule()
   * @generated
   */
  EAttribute getFormLayoutRule_MarginTop();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginWidth <em>Margin Width</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Margin Width</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getMarginWidth()
   * @see #getFormLayoutRule()
   * @generated
   */
  EAttribute getFormLayoutRule_MarginWidth();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getSpacing <em>Spacing</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Spacing</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule#getSpacing()
   * @see #getFormLayoutRule()
   * @generated
   */
  EAttribute getFormLayoutRule_Spacing();

  /**
   * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment <em>Grid Data Alignment</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for enum '<em>Grid Data Alignment</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment
   * @generated
   */
  EEnum getGridDataAlignment();

  /**
   * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment <em>To Sibling Alignment</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for enum '<em>To Sibling Alignment</em>'.
   * @see org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment
   * @generated
   */
  EEnum getToSiblingAlignment();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  SWTStylesFactory getSWTStylesFactory();

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
  interface Literals
  {
    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.RowLayoutRuleImpl <em>Row Layout Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.RowLayoutRuleImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getRowLayoutRule()
     * @generated
     */
    EClass ROW_LAYOUT_RULE = eINSTANCE.getRowLayoutRule();

    /**
     * The meta object literal for the '<em><b>Center</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__CENTER = eINSTANCE.getRowLayoutRule_Center();

    /**
     * The meta object literal for the '<em><b>Fill</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__FILL = eINSTANCE.getRowLayoutRule_Fill();

    /**
     * The meta object literal for the '<em><b>Justify</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__JUSTIFY = eINSTANCE.getRowLayoutRule_Justify();

    /**
     * The meta object literal for the '<em><b>Margin Bottom</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__MARGIN_BOTTOM = eINSTANCE.getRowLayoutRule_MarginBottom();

    /**
     * The meta object literal for the '<em><b>Margin Height</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__MARGIN_HEIGHT = eINSTANCE.getRowLayoutRule_MarginHeight();

    /**
     * The meta object literal for the '<em><b>Margin Left</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__MARGIN_LEFT = eINSTANCE.getRowLayoutRule_MarginLeft();

    /**
     * The meta object literal for the '<em><b>Margin Right</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__MARGIN_RIGHT = eINSTANCE.getRowLayoutRule_MarginRight();

    /**
     * The meta object literal for the '<em><b>Margin Top</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__MARGIN_TOP = eINSTANCE.getRowLayoutRule_MarginTop();

    /**
     * The meta object literal for the '<em><b>Margin Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__MARGIN_WIDTH = eINSTANCE.getRowLayoutRule_MarginWidth();

    /**
     * The meta object literal for the '<em><b>Pack</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__PACK = eINSTANCE.getRowLayoutRule_Pack();

    /**
     * The meta object literal for the '<em><b>Spacing</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__SPACING = eINSTANCE.getRowLayoutRule_Spacing();

    /**
     * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__TYPE = eINSTANCE.getRowLayoutRule_Type();

    /**
     * The meta object literal for the '<em><b>Wrap</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_LAYOUT_RULE__WRAP = eINSTANCE.getRowLayoutRule_Wrap();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.RowDataRuleImpl <em>Row Data Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.RowDataRuleImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getRowDataRule()
     * @generated
     */
    EClass ROW_DATA_RULE = eINSTANCE.getRowDataRule();

    /**
     * The meta object literal for the '<em><b>Exclude</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_DATA_RULE__EXCLUDE = eINSTANCE.getRowDataRule_Exclude();

    /**
     * The meta object literal for the '<em><b>Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_DATA_RULE__WIDTH = eINSTANCE.getRowDataRule_Width();

    /**
     * The meta object literal for the '<em><b>Height</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ROW_DATA_RULE__HEIGHT = eINSTANCE.getRowDataRule_Height();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridLayoutRuleImpl <em>Grid Layout Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.GridLayoutRuleImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getGridLayoutRule()
     * @generated
     */
    EClass GRID_LAYOUT_RULE = eINSTANCE.getGridLayoutRule();

    /**
     * The meta object literal for the '<em><b>Horizontal Spacing</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__HORIZONTAL_SPACING = eINSTANCE.getGridLayoutRule_HorizontalSpacing();

    /**
     * The meta object literal for the '<em><b>Make Columns Equal Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__MAKE_COLUMNS_EQUAL_WIDTH = eINSTANCE.getGridLayoutRule_MakeColumnsEqualWidth();

    /**
     * The meta object literal for the '<em><b>Margin Bottom</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__MARGIN_BOTTOM = eINSTANCE.getGridLayoutRule_MarginBottom();

    /**
     * The meta object literal for the '<em><b>Margin Height</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__MARGIN_HEIGHT = eINSTANCE.getGridLayoutRule_MarginHeight();

    /**
     * The meta object literal for the '<em><b>Margin Left</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__MARGIN_LEFT = eINSTANCE.getGridLayoutRule_MarginLeft();

    /**
     * The meta object literal for the '<em><b>Margin Right</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__MARGIN_RIGHT = eINSTANCE.getGridLayoutRule_MarginRight();

    /**
     * The meta object literal for the '<em><b>Margin Top</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__MARGIN_TOP = eINSTANCE.getGridLayoutRule_MarginTop();

    /**
     * The meta object literal for the '<em><b>Margin Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__MARGIN_WIDTH = eINSTANCE.getGridLayoutRule_MarginWidth();

    /**
     * The meta object literal for the '<em><b>Num Columns</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__NUM_COLUMNS = eINSTANCE.getGridLayoutRule_NumColumns();

    /**
     * The meta object literal for the '<em><b>Vertical Spacing</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_LAYOUT_RULE__VERTICAL_SPACING = eINSTANCE.getGridLayoutRule_VerticalSpacing();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl <em>Grid Data Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getGridDataRule()
     * @generated
     */
    EClass GRID_DATA_RULE = eINSTANCE.getGridDataRule();

    /**
     * The meta object literal for the '<em><b>Exclude</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__EXCLUDE = eINSTANCE.getGridDataRule_Exclude();

    /**
     * The meta object literal for the '<em><b>Grab Excess Horizontal Space</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE = eINSTANCE.getGridDataRule_GrabExcessHorizontalSpace();

    /**
     * The meta object literal for the '<em><b>Grab Excess Vertical Space</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE = eINSTANCE.getGridDataRule_GrabExcessVerticalSpace();

    /**
     * The meta object literal for the '<em><b>Height Hint</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__HEIGHT_HINT = eINSTANCE.getGridDataRule_HeightHint();

    /**
     * The meta object literal for the '<em><b>Horizontal Alignement</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT = eINSTANCE.getGridDataRule_HorizontalAlignement();

    /**
     * The meta object literal for the '<em><b>Horizontal Indent</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__HORIZONTAL_INDENT = eINSTANCE.getGridDataRule_HorizontalIndent();

    /**
     * The meta object literal for the '<em><b>Horizontal Span</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__HORIZONTAL_SPAN = eINSTANCE.getGridDataRule_HorizontalSpan();

    /**
     * The meta object literal for the '<em><b>Minimum Height</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__MINIMUM_HEIGHT = eINSTANCE.getGridDataRule_MinimumHeight();

    /**
     * The meta object literal for the '<em><b>Minimum Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__MINIMUM_WIDTH = eINSTANCE.getGridDataRule_MinimumWidth();

    /**
     * The meta object literal for the '<em><b>Vertical Alignement</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__VERTICAL_ALIGNEMENT = eINSTANCE.getGridDataRule_VerticalAlignement();

    /**
     * The meta object literal for the '<em><b>Vertical Indent</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__VERTICAL_INDENT = eINSTANCE.getGridDataRule_VerticalIndent();

    /**
     * The meta object literal for the '<em><b>Vertical Span</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__VERTICAL_SPAN = eINSTANCE.getGridDataRule_VerticalSpan();

    /**
     * The meta object literal for the '<em><b>Width Hint</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute GRID_DATA_RULE__WIDTH_HINT = eINSTANCE.getGridDataRule_WidthHint();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl <em>Fill Layout Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFillLayoutRule()
     * @generated
     */
    EClass FILL_LAYOUT_RULE = eINSTANCE.getFillLayoutRule();

    /**
     * The meta object literal for the '<em><b>Margin Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FILL_LAYOUT_RULE__MARGIN_WIDTH = eINSTANCE.getFillLayoutRule_MarginWidth();

    /**
     * The meta object literal for the '<em><b>Margin Height</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FILL_LAYOUT_RULE__MARGIN_HEIGHT = eINSTANCE.getFillLayoutRule_MarginHeight();

    /**
     * The meta object literal for the '<em><b>Spacing</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FILL_LAYOUT_RULE__SPACING = eINSTANCE.getFillLayoutRule_Spacing();

    /**
     * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FILL_LAYOUT_RULE__TYPE = eINSTANCE.getFillLayoutRule_Type();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.FormAttachment <em>Form Attachment</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.FormAttachment
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFormAttachment()
     * @generated
     */
    EClass FORM_ATTACHMENT = eINSTANCE.getFormAttachment();

    /**
     * The meta object literal for the '<em><b>Offset</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_ATTACHMENT__OFFSET = eINSTANCE.getFormAttachment_Offset();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToSiblingImpl <em>Attachment To Sibling</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToSiblingImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getAttachmentToSibling()
     * @generated
     */
    EClass ATTACHMENT_TO_SIBLING = eINSTANCE.getAttachmentToSibling();

    /**
     * The meta object literal for the '<em><b>Sibling Id</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ATTACHMENT_TO_SIBLING__SIBLING_ID = eINSTANCE.getAttachmentToSibling_SiblingId();

    /**
     * The meta object literal for the '<em><b>Alignment</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ATTACHMENT_TO_SIBLING__ALIGNMENT = eINSTANCE.getAttachmentToSibling_Alignment();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToContainerImpl <em>Attachment To Container</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToContainerImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getAttachmentToContainer()
     * @generated
     */
    EClass ATTACHMENT_TO_CONTAINER = eINSTANCE.getAttachmentToContainer();

    /**
     * The meta object literal for the '<em><b>Numerator</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ATTACHMENT_TO_CONTAINER__NUMERATOR = eINSTANCE.getAttachmentToContainer_Numerator();

    /**
     * The meta object literal for the '<em><b>Denominator</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute ATTACHMENT_TO_CONTAINER__DENOMINATOR = eINSTANCE.getAttachmentToContainer_Denominator();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.FormDataRuleImpl <em>Form Data Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.FormDataRuleImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFormDataRule()
     * @generated
     */
    EClass FORM_DATA_RULE = eINSTANCE.getFormDataRule();

    /**
     * The meta object literal for the '<em><b>Bottom</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FORM_DATA_RULE__BOTTOM = eINSTANCE.getFormDataRule_Bottom();

    /**
     * The meta object literal for the '<em><b>Left</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FORM_DATA_RULE__LEFT = eINSTANCE.getFormDataRule_Left();

    /**
     * The meta object literal for the '<em><b>Right</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FORM_DATA_RULE__RIGHT = eINSTANCE.getFormDataRule_Right();

    /**
     * The meta object literal for the '<em><b>Top</b></em>' containment reference feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference FORM_DATA_RULE__TOP = eINSTANCE.getFormDataRule_Top();

    /**
     * The meta object literal for the '<em><b>Height</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_DATA_RULE__HEIGHT = eINSTANCE.getFormDataRule_Height();

    /**
     * The meta object literal for the '<em><b>Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_DATA_RULE__WIDTH = eINSTANCE.getFormDataRule_Width();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.impl.FormLayoutRuleImpl <em>Form Layout Rule</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.FormLayoutRuleImpl
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getFormLayoutRule()
     * @generated
     */
    EClass FORM_LAYOUT_RULE = eINSTANCE.getFormLayoutRule();

    /**
     * The meta object literal for the '<em><b>Margin Bottom</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_LAYOUT_RULE__MARGIN_BOTTOM = eINSTANCE.getFormLayoutRule_MarginBottom();

    /**
     * The meta object literal for the '<em><b>Margin Height</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_LAYOUT_RULE__MARGIN_HEIGHT = eINSTANCE.getFormLayoutRule_MarginHeight();

    /**
     * The meta object literal for the '<em><b>Margin Left</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_LAYOUT_RULE__MARGIN_LEFT = eINSTANCE.getFormLayoutRule_MarginLeft();

    /**
     * The meta object literal for the '<em><b>Margin Right</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_LAYOUT_RULE__MARGIN_RIGHT = eINSTANCE.getFormLayoutRule_MarginRight();

    /**
     * The meta object literal for the '<em><b>Margin Top</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_LAYOUT_RULE__MARGIN_TOP = eINSTANCE.getFormLayoutRule_MarginTop();

    /**
     * The meta object literal for the '<em><b>Margin Width</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_LAYOUT_RULE__MARGIN_WIDTH = eINSTANCE.getFormLayoutRule_MarginWidth();

    /**
     * The meta object literal for the '<em><b>Spacing</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute FORM_LAYOUT_RULE__SPACING = eINSTANCE.getFormLayoutRule_Spacing();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment <em>Grid Data Alignment</em>}' enum.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getGridDataAlignment()
     * @generated
     */
    EEnum GRID_DATA_ALIGNMENT = eINSTANCE.getGridDataAlignment();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment <em>To Sibling Alignment</em>}' enum.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment
     * @see org.eclipse.wazaabi.mm.swt.styles.impl.SWTStylesPackageImpl#getToSiblingAlignment()
     * @generated
     */
    EEnum TO_SIBLING_ALIGNMENT = eINSTANCE.getToSiblingAlignment();

  }

} //SWTStylesPackage
