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
 * A representation of the model object '<em><b>Bar Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.BarLayoutRule#isDraggable <em>Draggable</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBarLayoutRule()
 * @model
 * @generated
 */
public interface BarLayoutRule extends LayoutRule {
	/**
	 * Returns the value of the '<em><b>Draggable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Draggable</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Draggable</em>' attribute.
	 * @see #isSetDraggable()
	 * @see #unsetDraggable()
	 * @see #setDraggable(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBarLayoutRule_Draggable()
	 * @model unsettable="true" required="true"
	 * @generated
	 */
	boolean isDraggable();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.BarLayoutRule#isDraggable <em>Draggable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Draggable</em>' attribute.
	 * @see #isSetDraggable()
	 * @see #unsetDraggable()
	 * @see #isDraggable()
	 * @generated
	 */
	void setDraggable(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.BarLayoutRule#isDraggable <em>Draggable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetDraggable()
	 * @see #isDraggable()
	 * @see #setDraggable(boolean)
	 * @generated
	 */
	void unsetDraggable();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.BarLayoutRule#isDraggable <em>Draggable</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Draggable</em>' attribute is set.
	 * @see #unsetDraggable()
	 * @see #isDraggable()
	 * @see #setDraggable(boolean)
	 * @generated
	 */
	boolean isSetDraggable();

} // BarLayoutRule
