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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Stack Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginHeight <em>Margin Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginWidth <em>Margin Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getTop <em>Top</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getStackLayoutRule()
 * @model
 * @generated
 */
public interface StackLayoutRule extends LayoutRule {
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
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getStackLayoutRule_MarginHeight()
	 * @model default="0"
	 * @generated
	 */
	int getMarginHeight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginHeight <em>Margin Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Height</em>' attribute.
	 * @see #getMarginHeight()
	 * @generated
	 */
	void setMarginHeight(int value);

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
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getStackLayoutRule_MarginWidth()
	 * @model default="0"
	 * @generated
	 */
	int getMarginWidth();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getMarginWidth <em>Margin Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Margin Width</em>' attribute.
	 * @see #getMarginWidth()
	 * @generated
	 */
	void setMarginWidth(int value);

	/**
	 * Returns the value of the '<em><b>Top</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Top</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Top</em>' attribute.
	 * @see #setTop(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getStackLayoutRule_Top()
	 * @model
	 * @generated
	 */
	int getTop();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule#getTop <em>Top</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Top</em>' attribute.
	 * @see #getTop()
	 * @generated
	 */
	void setTop(int value);

} // StackLayoutRule
