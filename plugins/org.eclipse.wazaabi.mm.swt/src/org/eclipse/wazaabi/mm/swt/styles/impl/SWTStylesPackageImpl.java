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
package org.eclipse.wazaabi.mm.swt.styles.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

import org.eclipse.wazaabi.mm.swt.descriptors.impl.SWTDescriptorsPackageImpl;

import org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer;
import org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.FormAttachment;
import org.eclipse.wazaabi.mm.swt.styles.FormDataRule;
import org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class SWTStylesPackageImpl extends EPackageImpl implements SWTStylesPackage
{
  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass rowLayoutRuleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass rowDataRuleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass gridLayoutRuleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass gridDataRuleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass fillLayoutRuleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass formAttachmentEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass attachmentToSiblingEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass attachmentToContainerEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass formDataRuleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass formLayoutRuleEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EEnum gridDataAlignmentEEnum = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EEnum toSiblingAlignmentEEnum = null;

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
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#eNS_URI
   * @see #init()
   * @generated
   */
  private SWTStylesPackageImpl()
  {
    super(eNS_URI, SWTStylesFactory.eINSTANCE);
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
   * <p>This method is used to initialize {@link SWTStylesPackage#eINSTANCE} when that field is accessed.
   * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #eNS_URI
   * @see #createPackageContents()
   * @see #initializePackageContents()
   * @generated
   */
  public static SWTStylesPackage init()
  {
    if (isInited) return (SWTStylesPackage)EPackage.Registry.INSTANCE.getEPackage(SWTStylesPackage.eNS_URI);

    // Obtain or create and register package
    SWTStylesPackageImpl theSWTStylesPackage = (SWTStylesPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof SWTStylesPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new SWTStylesPackageImpl());

    isInited = true;

    // Initialize simple dependencies
    CorePackage.eINSTANCE.eClass();

    // Obtain or create and register interdependencies
    SWTDescriptorsPackageImpl theSWTDescriptorsPackage = (SWTDescriptorsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(SWTDescriptorsPackage.eNS_URI) instanceof SWTDescriptorsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(SWTDescriptorsPackage.eNS_URI) : SWTDescriptorsPackage.eINSTANCE);

    // Create package meta-data objects
    theSWTStylesPackage.createPackageContents();
    theSWTDescriptorsPackage.createPackageContents();

    // Initialize created meta-data
    theSWTStylesPackage.initializePackageContents();
    theSWTDescriptorsPackage.initializePackageContents();

    // Mark meta-data to indicate it can't be changed
    theSWTStylesPackage.freeze();

  
    // Update the registry and return the package
    EPackage.Registry.INSTANCE.put(SWTStylesPackage.eNS_URI, theSWTStylesPackage);
    return theSWTStylesPackage;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getRowLayoutRule()
  {
    return rowLayoutRuleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_Center()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_Fill()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_Justify()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_MarginBottom()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_MarginHeight()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_MarginLeft()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_MarginRight()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_MarginTop()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(7);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_MarginWidth()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(8);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_Pack()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(9);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_Spacing()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(10);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_Type()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(11);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowLayoutRule_Wrap()
  {
    return (EAttribute)rowLayoutRuleEClass.getEStructuralFeatures().get(12);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getRowDataRule()
  {
    return rowDataRuleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowDataRule_Exclude()
  {
    return (EAttribute)rowDataRuleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowDataRule_Width()
  {
    return (EAttribute)rowDataRuleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getRowDataRule_Height()
  {
    return (EAttribute)rowDataRuleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getGridLayoutRule()
  {
    return gridLayoutRuleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_HorizontalSpacing()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_MakeColumnsEqualWidth()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_MarginBottom()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_MarginHeight()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_MarginLeft()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_MarginRight()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_MarginTop()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_MarginWidth()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(7);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_NumColumns()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(8);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridLayoutRule_VerticalSpacing()
  {
    return (EAttribute)gridLayoutRuleEClass.getEStructuralFeatures().get(9);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getGridDataRule()
  {
    return gridDataRuleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_Exclude()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_GrabExcessHorizontalSpace()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_GrabExcessVerticalSpace()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_HeightHint()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_HorizontalAlignement()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_HorizontalIndent()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_HorizontalSpan()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_MinimumHeight()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(7);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_MinimumWidth()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(8);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_VerticalAlignement()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(9);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_VerticalIndent()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(10);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_VerticalSpan()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(11);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getGridDataRule_WidthHint()
  {
    return (EAttribute)gridDataRuleEClass.getEStructuralFeatures().get(12);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFillLayoutRule()
  {
    return fillLayoutRuleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFillLayoutRule_MarginWidth()
  {
    return (EAttribute)fillLayoutRuleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFillLayoutRule_MarginHeight()
  {
    return (EAttribute)fillLayoutRuleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFillLayoutRule_Spacing()
  {
    return (EAttribute)fillLayoutRuleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFillLayoutRule_Type()
  {
    return (EAttribute)fillLayoutRuleEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFormAttachment()
  {
    return formAttachmentEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormAttachment_Offset()
  {
    return (EAttribute)formAttachmentEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAttachmentToSibling()
  {
    return attachmentToSiblingEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAttachmentToSibling_SiblingId()
  {
    return (EAttribute)attachmentToSiblingEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAttachmentToSibling_Alignment()
  {
    return (EAttribute)attachmentToSiblingEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAttachmentToContainer()
  {
    return attachmentToContainerEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAttachmentToContainer_Numerator()
  {
    return (EAttribute)attachmentToContainerEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getAttachmentToContainer_Denominator()
  {
    return (EAttribute)attachmentToContainerEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFormDataRule()
  {
    return formDataRuleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFormDataRule_Bottom()
  {
    return (EReference)formDataRuleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFormDataRule_Left()
  {
    return (EReference)formDataRuleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFormDataRule_Right()
  {
    return (EReference)formDataRuleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getFormDataRule_Top()
  {
    return (EReference)formDataRuleEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormDataRule_Height()
  {
    return (EAttribute)formDataRuleEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormDataRule_Width()
  {
    return (EAttribute)formDataRuleEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getFormLayoutRule()
  {
    return formLayoutRuleEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormLayoutRule_MarginBottom()
  {
    return (EAttribute)formLayoutRuleEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormLayoutRule_MarginHeight()
  {
    return (EAttribute)formLayoutRuleEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormLayoutRule_MarginLeft()
  {
    return (EAttribute)formLayoutRuleEClass.getEStructuralFeatures().get(2);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormLayoutRule_MarginRight()
  {
    return (EAttribute)formLayoutRuleEClass.getEStructuralFeatures().get(3);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormLayoutRule_MarginTop()
  {
    return (EAttribute)formLayoutRuleEClass.getEStructuralFeatures().get(4);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormLayoutRule_MarginWidth()
  {
    return (EAttribute)formLayoutRuleEClass.getEStructuralFeatures().get(5);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getFormLayoutRule_Spacing()
  {
    return (EAttribute)formLayoutRuleEClass.getEStructuralFeatures().get(6);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EEnum getGridDataAlignment()
  {
    return gridDataAlignmentEEnum;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EEnum getToSiblingAlignment()
  {
    return toSiblingAlignmentEEnum;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SWTStylesFactory getSWTStylesFactory()
  {
    return (SWTStylesFactory)getEFactoryInstance();
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
  public void createPackageContents()
  {
    if (isCreated) return;
    isCreated = true;

    // Create classes and their features
    rowLayoutRuleEClass = createEClass(ROW_LAYOUT_RULE);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__CENTER);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__FILL);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__JUSTIFY);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__MARGIN_BOTTOM);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__MARGIN_HEIGHT);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__MARGIN_LEFT);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__MARGIN_RIGHT);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__MARGIN_TOP);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__MARGIN_WIDTH);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__PACK);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__SPACING);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__TYPE);
    createEAttribute(rowLayoutRuleEClass, ROW_LAYOUT_RULE__WRAP);

    rowDataRuleEClass = createEClass(ROW_DATA_RULE);
    createEAttribute(rowDataRuleEClass, ROW_DATA_RULE__EXCLUDE);
    createEAttribute(rowDataRuleEClass, ROW_DATA_RULE__WIDTH);
    createEAttribute(rowDataRuleEClass, ROW_DATA_RULE__HEIGHT);

    gridLayoutRuleEClass = createEClass(GRID_LAYOUT_RULE);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__HORIZONTAL_SPACING);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__MAKE_COLUMNS_EQUAL_WIDTH);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__MARGIN_BOTTOM);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__MARGIN_HEIGHT);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__MARGIN_LEFT);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__MARGIN_RIGHT);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__MARGIN_TOP);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__MARGIN_WIDTH);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__NUM_COLUMNS);
    createEAttribute(gridLayoutRuleEClass, GRID_LAYOUT_RULE__VERTICAL_SPACING);

    gridDataRuleEClass = createEClass(GRID_DATA_RULE);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__EXCLUDE);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__HEIGHT_HINT);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__HORIZONTAL_INDENT);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__HORIZONTAL_SPAN);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__MINIMUM_HEIGHT);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__MINIMUM_WIDTH);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__VERTICAL_ALIGNEMENT);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__VERTICAL_INDENT);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__VERTICAL_SPAN);
    createEAttribute(gridDataRuleEClass, GRID_DATA_RULE__WIDTH_HINT);

    fillLayoutRuleEClass = createEClass(FILL_LAYOUT_RULE);
    createEAttribute(fillLayoutRuleEClass, FILL_LAYOUT_RULE__MARGIN_WIDTH);
    createEAttribute(fillLayoutRuleEClass, FILL_LAYOUT_RULE__MARGIN_HEIGHT);
    createEAttribute(fillLayoutRuleEClass, FILL_LAYOUT_RULE__SPACING);
    createEAttribute(fillLayoutRuleEClass, FILL_LAYOUT_RULE__TYPE);

    formAttachmentEClass = createEClass(FORM_ATTACHMENT);
    createEAttribute(formAttachmentEClass, FORM_ATTACHMENT__OFFSET);

    attachmentToSiblingEClass = createEClass(ATTACHMENT_TO_SIBLING);
    createEAttribute(attachmentToSiblingEClass, ATTACHMENT_TO_SIBLING__SIBLING_ID);
    createEAttribute(attachmentToSiblingEClass, ATTACHMENT_TO_SIBLING__ALIGNMENT);

    attachmentToContainerEClass = createEClass(ATTACHMENT_TO_CONTAINER);
    createEAttribute(attachmentToContainerEClass, ATTACHMENT_TO_CONTAINER__NUMERATOR);
    createEAttribute(attachmentToContainerEClass, ATTACHMENT_TO_CONTAINER__DENOMINATOR);

    formDataRuleEClass = createEClass(FORM_DATA_RULE);
    createEReference(formDataRuleEClass, FORM_DATA_RULE__BOTTOM);
    createEReference(formDataRuleEClass, FORM_DATA_RULE__LEFT);
    createEReference(formDataRuleEClass, FORM_DATA_RULE__RIGHT);
    createEReference(formDataRuleEClass, FORM_DATA_RULE__TOP);
    createEAttribute(formDataRuleEClass, FORM_DATA_RULE__HEIGHT);
    createEAttribute(formDataRuleEClass, FORM_DATA_RULE__WIDTH);

    formLayoutRuleEClass = createEClass(FORM_LAYOUT_RULE);
    createEAttribute(formLayoutRuleEClass, FORM_LAYOUT_RULE__MARGIN_BOTTOM);
    createEAttribute(formLayoutRuleEClass, FORM_LAYOUT_RULE__MARGIN_HEIGHT);
    createEAttribute(formLayoutRuleEClass, FORM_LAYOUT_RULE__MARGIN_LEFT);
    createEAttribute(formLayoutRuleEClass, FORM_LAYOUT_RULE__MARGIN_RIGHT);
    createEAttribute(formLayoutRuleEClass, FORM_LAYOUT_RULE__MARGIN_TOP);
    createEAttribute(formLayoutRuleEClass, FORM_LAYOUT_RULE__MARGIN_WIDTH);
    createEAttribute(formLayoutRuleEClass, FORM_LAYOUT_RULE__SPACING);

    // Create enums
    gridDataAlignmentEEnum = createEEnum(GRID_DATA_ALIGNMENT);
    toSiblingAlignmentEEnum = createEEnum(TO_SIBLING_ALIGNMENT);
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
  public void initializePackageContents()
  {
    if (isInitialized) return;
    isInitialized = true;

    // Initialize package
    setName(eNAME);
    setNsPrefix(eNS_PREFIX);
    setNsURI(eNS_URI);

    // Obtain other dependent packages
    CoreStylesPackage theCoreStylesPackage = (CoreStylesPackage)EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI);
    CorePackage theCorePackage = (CorePackage)EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI);

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    rowLayoutRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutRule());
    rowDataRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutDataRule());
    gridLayoutRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutRule());
    gridDataRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutDataRule());
    fillLayoutRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutRule());
    attachmentToSiblingEClass.getESuperTypes().add(this.getFormAttachment());
    attachmentToContainerEClass.getESuperTypes().add(this.getFormAttachment());
    formDataRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutDataRule());
    formLayoutRuleEClass.getESuperTypes().add(theCoreStylesPackage.getLayoutRule());

    // Initialize classes and features; add operations and parameters
    initEClass(rowLayoutRuleEClass, RowLayoutRule.class, "RowLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getRowLayoutRule_Center(), ecorePackage.getEBoolean(), "center", "false", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_Fill(), ecorePackage.getEBoolean(), "fill", "false", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_Justify(), ecorePackage.getEBoolean(), "justify", "false", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_MarginBottom(), ecorePackage.getEInt(), "marginBottom", "3", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_MarginHeight(), ecorePackage.getEInt(), "marginHeight", "0", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_MarginLeft(), ecorePackage.getEInt(), "marginLeft", "3", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_MarginRight(), ecorePackage.getEInt(), "marginRight", "3", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_MarginTop(), ecorePackage.getEInt(), "marginTop", "3", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_MarginWidth(), ecorePackage.getEInt(), "marginWidth", "0", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_Pack(), ecorePackage.getEBoolean(), "pack", "true", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_Spacing(), ecorePackage.getEInt(), "spacing", "3", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_Type(), theCorePackage.getOrientation(), "type", "HORIZONTAL", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowLayoutRule_Wrap(), ecorePackage.getEBoolean(), "wrap", "true", 0, 1, RowLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(rowDataRuleEClass, RowDataRule.class, "RowDataRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getRowDataRule_Exclude(), ecorePackage.getEBoolean(), "exclude", "false", 0, 1, RowDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowDataRule_Width(), ecorePackage.getEInt(), "width", "-1", 0, 1, RowDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getRowDataRule_Height(), ecorePackage.getEInt(), "height", "-1", 0, 1, RowDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(gridLayoutRuleEClass, GridLayoutRule.class, "GridLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getGridLayoutRule_HorizontalSpacing(), ecorePackage.getEInt(), "horizontalSpacing", "5", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_MakeColumnsEqualWidth(), ecorePackage.getEBoolean(), "makeColumnsEqualWidth", "false", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_MarginBottom(), ecorePackage.getEInt(), "marginBottom", "0", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_MarginHeight(), ecorePackage.getEInt(), "marginHeight", "5", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_MarginLeft(), ecorePackage.getEInt(), "marginLeft", "0", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_MarginRight(), ecorePackage.getEInt(), "marginRight", "0", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_MarginTop(), ecorePackage.getEInt(), "marginTop", "0", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_MarginWidth(), ecorePackage.getEInt(), "marginWidth", "5", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_NumColumns(), ecorePackage.getEInt(), "numColumns", "1", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridLayoutRule_VerticalSpacing(), ecorePackage.getEInt(), "verticalSpacing", "5", 0, 1, GridLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(gridDataRuleEClass, GridDataRule.class, "GridDataRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getGridDataRule_Exclude(), ecorePackage.getEBoolean(), "exclude", "false", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_GrabExcessHorizontalSpace(), ecorePackage.getEBoolean(), "grabExcessHorizontalSpace", "false", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_GrabExcessVerticalSpace(), ecorePackage.getEBoolean(), "grabExcessVerticalSpace", "false", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_HeightHint(), ecorePackage.getEInt(), "heightHint", "-1", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_HorizontalAlignement(), this.getGridDataAlignment(), "horizontalAlignement", null, 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_HorizontalIndent(), ecorePackage.getEInt(), "horizontalIndent", "0", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_HorizontalSpan(), ecorePackage.getEInt(), "horizontalSpan", "1", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_MinimumHeight(), ecorePackage.getEInt(), "minimumHeight", "-1", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_MinimumWidth(), ecorePackage.getEInt(), "minimumWidth", "-1", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_VerticalAlignement(), this.getGridDataAlignment(), "verticalAlignement", null, 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_VerticalIndent(), ecorePackage.getEInt(), "verticalIndent", "0", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_VerticalSpan(), ecorePackage.getEInt(), "verticalSpan", "1", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getGridDataRule_WidthHint(), ecorePackage.getEInt(), "widthHint", "-1", 0, 1, GridDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(fillLayoutRuleEClass, FillLayoutRule.class, "FillLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getFillLayoutRule_MarginWidth(), ecorePackage.getEInt(), "marginWidth", "0", 0, 1, FillLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFillLayoutRule_MarginHeight(), ecorePackage.getEInt(), "marginHeight", "0", 0, 1, FillLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFillLayoutRule_Spacing(), ecorePackage.getEInt(), "spacing", "0", 0, 1, FillLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFillLayoutRule_Type(), theCorePackage.getOrientation(), "type", "HORIZONTAL", 0, 1, FillLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(formAttachmentEClass, FormAttachment.class, "FormAttachment", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getFormAttachment_Offset(), ecorePackage.getEInt(), "offset", "0", 0, 1, FormAttachment.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(attachmentToSiblingEClass, AttachmentToSibling.class, "AttachmentToSibling", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getAttachmentToSibling_SiblingId(), ecorePackage.getEString(), "siblingId", null, 0, 1, AttachmentToSibling.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getAttachmentToSibling_Alignment(), this.getToSiblingAlignment(), "alignment", "DEFAULT", 0, 1, AttachmentToSibling.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(attachmentToContainerEClass, AttachmentToContainer.class, "AttachmentToContainer", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getAttachmentToContainer_Numerator(), ecorePackage.getEInt(), "numerator", null, 0, 1, AttachmentToContainer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getAttachmentToContainer_Denominator(), ecorePackage.getEInt(), "denominator", "100", 0, 1, AttachmentToContainer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(formDataRuleEClass, FormDataRule.class, "FormDataRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getFormDataRule_Bottom(), this.getFormAttachment(), null, "bottom", null, 0, 1, FormDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getFormDataRule_Left(), this.getFormAttachment(), null, "left", null, 0, 1, FormDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getFormDataRule_Right(), this.getFormAttachment(), null, "right", null, 0, 1, FormDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getFormDataRule_Top(), this.getFormAttachment(), null, "top", null, 0, 1, FormDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormDataRule_Height(), ecorePackage.getEInt(), "height", "-1", 0, 1, FormDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormDataRule_Width(), ecorePackage.getEInt(), "width", "-1", 0, 1, FormDataRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(formLayoutRuleEClass, FormLayoutRule.class, "FormLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getFormLayoutRule_MarginBottom(), ecorePackage.getEInt(), "marginBottom", "0", 0, 1, FormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormLayoutRule_MarginHeight(), ecorePackage.getEInt(), "marginHeight", "0", 0, 1, FormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormLayoutRule_MarginLeft(), ecorePackage.getEInt(), "marginLeft", "0", 0, 1, FormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormLayoutRule_MarginRight(), ecorePackage.getEInt(), "marginRight", "0", 0, 1, FormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormLayoutRule_MarginTop(), ecorePackage.getEInt(), "marginTop", "0", 0, 1, FormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormLayoutRule_MarginWidth(), ecorePackage.getEInt(), "marginWidth", "0", 0, 1, FormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getFormLayoutRule_Spacing(), ecorePackage.getEInt(), "spacing", "0", 0, 1, FormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    // Initialize enums and add enum literals
    initEEnum(gridDataAlignmentEEnum, GridDataAlignment.class, "GridDataAlignment");
    addEEnumLiteral(gridDataAlignmentEEnum, GridDataAlignment.BEGINNING);
    addEEnumLiteral(gridDataAlignmentEEnum, GridDataAlignment.CENTER);
    addEEnumLiteral(gridDataAlignmentEEnum, GridDataAlignment.END);
    addEEnumLiteral(gridDataAlignmentEEnum, GridDataAlignment.FILL);

    initEEnum(toSiblingAlignmentEEnum, ToSiblingAlignment.class, "ToSiblingAlignment");
    addEEnumLiteral(toSiblingAlignmentEEnum, ToSiblingAlignment.TOP);
    addEEnumLiteral(toSiblingAlignmentEEnum, ToSiblingAlignment.BOTTOM);
    addEEnumLiteral(toSiblingAlignmentEEnum, ToSiblingAlignment.LEFT);
    addEEnumLiteral(toSiblingAlignmentEEnum, ToSiblingAlignment.RIGHT);
    addEEnumLiteral(toSiblingAlignmentEEnum, ToSiblingAlignment.CENTER);
    addEEnumLiteral(toSiblingAlignmentEEnum, ToSiblingAlignment.DEFAULT);

    // Create resource
    createResource(eNS_URI);
  }

} //SWTStylesPackageImpl
