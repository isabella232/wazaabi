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

import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Row Data Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#isExclude <em>Exclude</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getWidth <em>Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getHeight <em>Height</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowDataRule()
 * @model
 * @generated
 */
public interface RowDataRule extends LayoutDataRule {
	/**
	 * Returns the value of the '<em><b>Exclude</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Exclude</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Exclude</em>' attribute.
	 * @see #setExclude(boolean)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowDataRule_Exclude()
	 * @model default="false"
	 * @generated
	 */
	boolean isExclude();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#isExclude <em>Exclude</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Exclude</em>' attribute.
	 * @see #isExclude()
	 * @generated
	 */
	void setExclude(boolean value);

	/**
	 * Returns the value of the '<em><b>Width</b></em>' attribute.
	 * The default value is <code>"-1"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Width</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Width</em>' attribute.
	 * @see #setWidth(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowDataRule_Width()
	 * @model default="-1"
	 * @generated
	 */
	int getWidth();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getWidth <em>Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Width</em>' attribute.
	 * @see #getWidth()
	 * @generated
	 */
	void setWidth(int value);

	/**
	 * Returns the value of the '<em><b>Height</b></em>' attribute.
	 * The default value is <code>"-1"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Height</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Height</em>' attribute.
	 * @see #setHeight(int)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getRowDataRule_Height()
	 * @model default="-1"
	 * @generated
	 */
	int getHeight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.RowDataRule#getHeight <em>Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Height</em>' attribute.
	 * @see #getHeight()
	 * @generated
	 */
	void setHeight(int value);

} // RowDataRule
