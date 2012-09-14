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
 * A representation of the model object '<em><b>Boolean Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.BooleanRule#isValue <em>Value</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBooleanRule()
 * @model
 * @generated
 */
public interface BooleanRule extends StyleRule {
	/**
	 * Returns the value of the '<em><b>Value</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Value</em>' attribute.
	 * @see #setValue(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBooleanRule_Value()
	 * @model default="false"
	 * @generated
	 */
	boolean isValue();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.BooleanRule#isValue <em>Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Value</em>' attribute.
	 * @see #isValue()
	 * @generated
	 */
	void setValue(boolean value);

} // BooleanRule
