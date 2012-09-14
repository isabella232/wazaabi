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
 * A representation of the model object '<em><b>Color Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getRed <em>Red</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getGreen <em>Green</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getBlue <em>Blue</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getColorRule()
 * @model
 * @generated
 */
public interface ColorRule extends StyleRule {
	/**
	 * Returns the value of the '<em><b>Red</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Red</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Red</em>' attribute.
	 * @see #setRed(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getColorRule_Red()
	 * @model
	 * @generated
	 */
	int getRed();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getRed <em>Red</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Red</em>' attribute.
	 * @see #getRed()
	 * @generated
	 */
	void setRed(int value);

	/**
	 * Returns the value of the '<em><b>Green</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Green</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Green</em>' attribute.
	 * @see #setGreen(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getColorRule_Green()
	 * @model
	 * @generated
	 */
	int getGreen();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getGreen <em>Green</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Green</em>' attribute.
	 * @see #getGreen()
	 * @generated
	 */
	void setGreen(int value);

	/**
	 * Returns the value of the '<em><b>Blue</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Blue</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Blue</em>' attribute.
	 * @see #setBlue(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getColorRule_Blue()
	 * @model
	 * @generated
	 */
	int getBlue();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule#getBlue <em>Blue</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Blue</em>' attribute.
	 * @see #getBlue()
	 * @generated
	 */
	void setBlue(int value);

} // ColorRule
