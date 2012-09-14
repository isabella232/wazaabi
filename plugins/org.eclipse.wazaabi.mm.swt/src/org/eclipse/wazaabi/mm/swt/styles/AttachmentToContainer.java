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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Attachment To Container</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getNumerator <em>Numerator</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getDenominator <em>Denominator</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getAttachmentToContainer()
 * @model
 * @generated
 */
public interface AttachmentToContainer extends FormAttachment
{
  /**
   * Returns the value of the '<em><b>Numerator</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Numerator</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Numerator</em>' attribute.
   * @see #setNumerator(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getAttachmentToContainer_Numerator()
   * @model
   * @generated
   */
  int getNumerator();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getNumerator <em>Numerator</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Numerator</em>' attribute.
   * @see #getNumerator()
   * @generated
   */
  void setNumerator(int value);

  /**
   * Returns the value of the '<em><b>Denominator</b></em>' attribute.
   * The default value is <code>"100"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Denominator</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Denominator</em>' attribute.
   * @see #setDenominator(int)
   * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getAttachmentToContainer_Denominator()
   * @model default="100"
   * @generated
   */
  int getDenominator();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToContainer#getDenominator <em>Denominator</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Denominator</em>' attribute.
   * @see #getDenominator()
   * @generated
   */
  void setDenominator(int value);

} // AttachmentToContainer
