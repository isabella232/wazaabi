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

import org.eclipse.wazaabi.mm.core.Orientation;

import org.eclipse.wazaabi.mm.core.styles.LayoutRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Fill Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginWidth <em>Margin Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginHeight <em>Margin Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getSpacing <em>Spacing</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getType <em>Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFillLayoutRule()
 * @model
 * @generated
 */
public interface FillLayoutRule extends LayoutRule {
	/**
	 * Returns the value of the '<em><b>Margin Width</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Width</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Width</em>' attribute.
	 * @see #setMarginWidth(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFillLayoutRule_MarginWidth()
	 * @model default="0"
	 * @generated
	 */
	int getMarginWidth();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginWidth <em>Margin Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Width</em>' attribute.
	 * @see #getMarginWidth()
	 * @generated
	 */
	void setMarginWidth(int value);

	/**
	 * Returns the value of the '<em><b>Margin Height</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Margin Height</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Margin Height</em>' attribute.
	 * @see #setMarginHeight(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFillLayoutRule_MarginHeight()
	 * @model default="0"
	 * @generated
	 */
	int getMarginHeight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getMarginHeight <em>Margin Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Height</em>' attribute.
	 * @see #getMarginHeight()
	 * @generated
	 */
	void setMarginHeight(int value);

	/**
	 * Returns the value of the '<em><b>Spacing</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Spacing</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Spacing</em>' attribute.
	 * @see #setSpacing(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFillLayoutRule_Spacing()
	 * @model default="0"
	 * @generated
	 */
	int getSpacing();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getSpacing <em>Spacing</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Spacing</em>' attribute.
	 * @see #getSpacing()
	 * @generated
	 */
	void setSpacing(int value);

	/**
	 * Returns the value of the '<em><b>Type</b></em>' attribute.
	 * The default value is <code>"HORIZONTAL"</code>.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.core.Orientation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @see #setType(Orientation)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFillLayoutRule_Type()
	 * @model default="HORIZONTAL"
	 * @generated
	 */
	Orientation getType();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @see #getType()
	 * @generated
	 */
	void setType(Orientation value);

} // FillLayoutRule
