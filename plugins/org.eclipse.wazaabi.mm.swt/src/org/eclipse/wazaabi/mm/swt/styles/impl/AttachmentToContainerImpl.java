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
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Attachment To Container</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToContainerImpl#getOffset <em>Offset</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToContainerImpl#getNumerator <em>Numerator</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToContainerImpl#getDenominator <em>Denominator</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class AttachmentToContainerImpl extends EObjectImpl implements AttachmentToContainer
{
  /**
   * The default value of the '{@link #getOffset() <em>Offset</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOffset()
   * @generated
   * @ordered
   */
  protected static final int OFFSET_EDEFAULT = 0;

  /**
   * The cached value of the '{@link #getOffset() <em>Offset</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getOffset()
   * @generated
   * @ordered
   */
  protected int offset = OFFSET_EDEFAULT;

  /**
   * The default value of the '{@link #getNumerator() <em>Numerator</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getNumerator()
   * @generated
   * @ordered
   */
  protected static final int NUMERATOR_EDEFAULT = 0;

  /**
   * The cached value of the '{@link #getNumerator() <em>Numerator</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getNumerator()
   * @generated
   * @ordered
   */
  protected int numerator = NUMERATOR_EDEFAULT;

  /**
   * The default value of the '{@link #getDenominator() <em>Denominator</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDenominator()
   * @generated
   * @ordered
   */
  protected static final int DENOMINATOR_EDEFAULT = 100;

  /**
   * The cached value of the '{@link #getDenominator() <em>Denominator</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDenominator()
   * @generated
   * @ordered
   */
  protected int denominator = DENOMINATOR_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected AttachmentToContainerImpl()
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
    return SWTStylesPackage.Literals.ATTACHMENT_TO_CONTAINER;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public int getOffset()
  {
    return offset;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setOffset(int newOffset)
  {
    int oldOffset = offset;
    offset = newOffset;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ATTACHMENT_TO_CONTAINER__OFFSET, oldOffset, offset));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public int getNumerator()
  {
    return numerator;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setNumerator(int newNumerator)
  {
    int oldNumerator = numerator;
    numerator = newNumerator;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ATTACHMENT_TO_CONTAINER__NUMERATOR, oldNumerator, numerator));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public int getDenominator()
  {
    return denominator;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDenominator(int newDenominator)
  {
    int oldDenominator = denominator;
    denominator = newDenominator;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ATTACHMENT_TO_CONTAINER__DENOMINATOR, oldDenominator, denominator));
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
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__OFFSET:
        return getOffset();
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__NUMERATOR:
        return getNumerator();
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__DENOMINATOR:
        return getDenominator();
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
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__OFFSET:
        setOffset((Integer)newValue);
        return;
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__NUMERATOR:
        setNumerator((Integer)newValue);
        return;
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__DENOMINATOR:
        setDenominator((Integer)newValue);
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
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__OFFSET:
        setOffset(OFFSET_EDEFAULT);
        return;
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__NUMERATOR:
        setNumerator(NUMERATOR_EDEFAULT);
        return;
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__DENOMINATOR:
        setDenominator(DENOMINATOR_EDEFAULT);
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
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__OFFSET:
        return offset != OFFSET_EDEFAULT;
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__NUMERATOR:
        return numerator != NUMERATOR_EDEFAULT;
      case SWTStylesPackage.ATTACHMENT_TO_CONTAINER__DENOMINATOR:
        return denominator != DENOMINATOR_EDEFAULT;
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
    result.append(" (offset: ");
    result.append(offset);
    result.append(", numerator: ");
    result.append(numerator);
    result.append(", denominator: ");
    result.append(denominator);
    result.append(')');
    return result.toString();
  }

} //AttachmentToContainerImpl
