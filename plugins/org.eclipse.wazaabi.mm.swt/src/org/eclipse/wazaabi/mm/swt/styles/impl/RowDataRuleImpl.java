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

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.styles.impl.LayoutDataRuleImpl;

import org.eclipse.wazaabi.mm.swt.styles.RowDataRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Row Data Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.RowDataRuleImpl#isExclude <em>Exclude</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.RowDataRuleImpl#getWidth <em>Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.RowDataRuleImpl#getHeight <em>Height</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RowDataRuleImpl extends LayoutDataRuleImpl implements RowDataRule
{
  /**
   * The default value of the '{@link #isExclude() <em>Exclude</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isExclude()
   * @generated
   * @ordered
   */
  protected static final boolean EXCLUDE_EDEFAULT = false;

  /**
   * The cached value of the '{@link #isExclude() <em>Exclude</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isExclude()
   * @generated
   * @ordered
   */
  protected boolean exclude = EXCLUDE_EDEFAULT;

  /**
   * The default value of the '{@link #getWidth() <em>Width</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getWidth()
   * @generated
   * @ordered
   */
  protected static final int WIDTH_EDEFAULT = -1;

  /**
   * The cached value of the '{@link #getWidth() <em>Width</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getWidth()
   * @generated
   * @ordered
   */
  protected int width = WIDTH_EDEFAULT;

  /**
   * The default value of the '{@link #getHeight() <em>Height</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getHeight()
   * @generated
   * @ordered
   */
  protected static final int HEIGHT_EDEFAULT = -1;

  /**
   * The cached value of the '{@link #getHeight() <em>Height</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getHeight()
   * @generated
   * @ordered
   */
  protected int height = HEIGHT_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected RowDataRuleImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SWTStylesPackage.Literals.ROW_DATA_RULE;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean isExclude()
  {
    return exclude;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setExclude(boolean newExclude)
  {
    boolean oldExclude = exclude;
    exclude = newExclude;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ROW_DATA_RULE__EXCLUDE, oldExclude, exclude));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public int getWidth()
  {
    return width;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setWidth(int newWidth)
  {
    int oldWidth = width;
    width = newWidth;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ROW_DATA_RULE__WIDTH, oldWidth, width));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public int getHeight()
  {
    return height;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setHeight(int newHeight)
  {
    int oldHeight = height;
    height = newHeight;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ROW_DATA_RULE__HEIGHT, oldHeight, height));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SWTStylesPackage.ROW_DATA_RULE__EXCLUDE:
        return isExclude();
      case SWTStylesPackage.ROW_DATA_RULE__WIDTH:
        return getWidth();
      case SWTStylesPackage.ROW_DATA_RULE__HEIGHT:
        return getHeight();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SWTStylesPackage.ROW_DATA_RULE__EXCLUDE:
        setExclude((Boolean)newValue);
        return;
      case SWTStylesPackage.ROW_DATA_RULE__WIDTH:
        setWidth((Integer)newValue);
        return;
      case SWTStylesPackage.ROW_DATA_RULE__HEIGHT:
        setHeight((Integer)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SWTStylesPackage.ROW_DATA_RULE__EXCLUDE:
        setExclude(EXCLUDE_EDEFAULT);
        return;
      case SWTStylesPackage.ROW_DATA_RULE__WIDTH:
        setWidth(WIDTH_EDEFAULT);
        return;
      case SWTStylesPackage.ROW_DATA_RULE__HEIGHT:
        setHeight(HEIGHT_EDEFAULT);
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SWTStylesPackage.ROW_DATA_RULE__EXCLUDE:
        return exclude != EXCLUDE_EDEFAULT;
      case SWTStylesPackage.ROW_DATA_RULE__WIDTH:
        return width != WIDTH_EDEFAULT;
      case SWTStylesPackage.ROW_DATA_RULE__HEIGHT:
        return height != HEIGHT_EDEFAULT;
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (exclude: ");
    result.append(exclude);
    result.append(", width: ");
    result.append(width);
    result.append(", height: ");
    result.append(height);
    result.append(')');
    return result.toString();
  }

} //RowDataRuleImpl
