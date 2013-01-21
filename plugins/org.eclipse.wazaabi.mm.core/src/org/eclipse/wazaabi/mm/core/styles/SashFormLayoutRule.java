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

import org.eclipse.wazaabi.mm.core.Orientation;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Sash Form Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule#getOrientation <em>Orientation</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getSashFormLayoutRule()
 * @model
 * @generated
 */
public interface SashFormLayoutRule extends LayoutRule {
	/**
	 * Returns the value of the '<em><b>Orientation</b></em>' attribute.
	 * The default value is <code>"VERTICAL"</code>.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.core.Orientation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Orientation</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Orientation</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @see #setOrientation(Orientation)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getSashFormLayoutRule_Orientation()
	 * @model default="VERTICAL"
	 * @generated
	 */
	Orientation getOrientation();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule#getOrientation <em>Orientation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Orientation</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @see #getOrientation()
	 * @generated
	 */
	void setOrientation(Orientation value);

} // SashFormLayoutRule
