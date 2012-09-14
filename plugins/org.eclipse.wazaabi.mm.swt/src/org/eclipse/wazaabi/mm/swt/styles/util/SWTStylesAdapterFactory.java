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
package org.eclipse.wazaabi.mm.swt.styles.util;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

import org.eclipse.wazaabi.mm.swt.styles.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage
 * @generated
 */
public class SWTStylesAdapterFactory extends AdapterFactoryImpl
{
  /**
   * The cached model package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static SWTStylesPackage modelPackage;

  /**
   * Creates an instance of the adapter factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public SWTStylesAdapterFactory()
  {
    if (modelPackage == null)
    {
      modelPackage = SWTStylesPackage.eINSTANCE;
    }
  }

  /**
   * Returns whether this factory is applicable for the type of the object.
   * <!-- begin-user-doc -->
   * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
   * <!-- end-user-doc -->
   * @return whether this factory is applicable for the type of the object.
   * @generated
   */
  @Override
  public boolean isFactoryForType(Object object)
  {
    if (object == modelPackage)
    {
      return true;
    }
    if (object instanceof EObject)
    {
      return ((EObject)object).eClass().getEPackage() == modelPackage;
    }
    return false;
  }

  /**
   * The switch that delegates to the <code>createXXX</code> methods.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SWTStylesSwitch<Adapter> modelSwitch =
    new SWTStylesSwitch<Adapter>()
    {
      @Override
      public Adapter caseRowLayoutRule(RowLayoutRule object)
      {
        return createRowLayoutRuleAdapter();
      }
      @Override
      public Adapter caseRowDataRule(RowDataRule object)
      {
        return createRowDataRuleAdapter();
      }
      @Override
      public Adapter caseGridLayoutRule(GridLayoutRule object)
      {
        return createGridLayoutRuleAdapter();
      }
      @Override
      public Adapter caseGridDataRule(GridDataRule object)
      {
        return createGridDataRuleAdapter();
      }
      @Override
      public Adapter caseFillLayoutRule(FillLayoutRule object)
      {
        return createFillLayoutRuleAdapter();
      }
      @Override
      public Adapter caseFormAttachment(FormAttachment object)
      {
        return createFormAttachmentAdapter();
      }
      @Override
      public Adapter caseAttachmentToSibling(AttachmentToSibling object)
      {
        return createAttachmentToSiblingAdapter();
      }
      @Override
      public Adapter caseAttachmentToContainer(AttachmentToContainer object)
      {
        return createAttachmentToContainerAdapter();
      }
      @Override
      public Adapter caseFormDataRule(FormDataRule object)
      {
        return createFormDataRuleAdapter();
      }
      @Override
      public Adapter caseFormLayoutRule(FormLayoutRule object)
      {
        return createFormLayoutRuleAdapter();
      }
      @Override
      public Adapter caseStyleRule(StyleRule object)
      {
        return createStyleRuleAdapter();
      }
      @Override
      public Adapter caseLayoutRule(LayoutRule object)
      {
        return createLayoutRuleAdapter();
      }
      @Override
      public Adapter caseLayoutDataRule(LayoutDataRule object)
      {
        return createLayoutDataRuleAdapter();
      }
      @Override
      public Adapter defaultCase(EObject object)
      {
        return createEObjectAdapter();
      }
    };

  /**
   * Creates an adapter for the <code>target</code>.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param target the object to adapt.
   * @return the adapter for the <code>target</code>.
   * @generated
   */
  @Override
  public Adapter createAdapter(Notifier target)
  {
    return modelSwitch.doSwitch((EObject)target);
  }


  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule <em>Row Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowLayoutRule
   * @generated
   */
  public Adapter createRowLayoutRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule <em>Row Data Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.RowDataRule
   * @generated
   */
  public Adapter createRowDataRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule <em>Grid Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule
   * @generated
   */
  public Adapter createGridLayoutRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.GridDataRule <em>Grid Data Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.GridDataRule
   * @generated
   */
  public Adapter createGridDataRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule <em>Fill Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule
   * @generated
   */
  public Adapter createFillLayoutRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.FormAttachment <em>Form Attachment</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormAttachment
   * @generated
   */
  public Adapter createFormAttachmentAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling <em>Attachment To Sibling</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling
   * @generated
   */
  public Adapter createAttachmentToSiblingAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer <em>Attachment To Container</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer
   * @generated
   */
  public Adapter createAttachmentToContainerAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule <em>Form Data Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormDataRule
   * @generated
   */
  public Adapter createFormDataRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule <em>Form Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.swt.styles.FormLayoutRule
   * @generated
   */
  public Adapter createFormLayoutRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.StyleRule <em>Style Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.core.styles.StyleRule
   * @generated
   */
  public Adapter createStyleRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.LayoutRule <em>Layout Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.core.styles.LayoutRule
   * @generated
   */
  public Adapter createLayoutRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.LayoutDataRule <em>Layout Data Rule</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.core.styles.LayoutDataRule
   * @generated
   */
  public Adapter createLayoutDataRuleAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for the default case.
   * <!-- begin-user-doc -->
   * This default implementation returns null.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @generated
   */
  public Adapter createEObjectAdapter()
  {
    return null;
  }

} //SWTStylesAdapterFactory
