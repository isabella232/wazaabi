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
 * A representation of the model object '<em><b>Box Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule#getOrientation <em>Orientation</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule#getMargin <em>Margin</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule#getSpacing <em>Spacing</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBoxLayoutRule()
 * @model
 * @generated
 */
public interface BoxLayoutRule extends LayoutRule {
    /**
     * Returns the value of the '<em><b>Orientation</b></em>' attribute.
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
     * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBoxLayoutRule_Orientation()
     * @model
     * @generated
     */
    Orientation getOrientation();

    /**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule#getOrientation <em>Orientation</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Orientation</em>' attribute.
     * @see org.eclipse.wazaabi.mm.core.Orientation
     * @see #getOrientation()
     * @generated
     */
    void setOrientation(Orientation value);

    /**
     * Returns the value of the '<em><b>Margin</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <p>
     * If the meaning of the '<em>Margin</em>' attribute isn't clear,
     * there really should be more of a description here...
     * </p>
     * <!-- end-user-doc -->
     * @return the value of the '<em>Margin</em>' attribute.
     * @see #setMargin(int)
     * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBoxLayoutRule_Margin()
     * @model
     * @generated
     */
    int getMargin();

    /**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule#getMargin <em>Margin</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Margin</em>' attribute.
     * @see #getMargin()
     * @generated
     */
    void setMargin(int value);

    /**
     * Returns the value of the '<em><b>Spacing</b></em>' attribute.
     * <!-- begin-user-doc -->
     * <p>
     * If the meaning of the '<em>Spacing</em>' attribute isn't clear,
     * there really should be more of a description here...
     * </p>
     * <!-- end-user-doc -->
     * @return the value of the '<em>Spacing</em>' attribute.
     * @see #setSpacing(int)
     * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getBoxLayoutRule_Spacing()
     * @model
     * @generated
     */
    int getSpacing();

    /**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule#getSpacing <em>Spacing</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Spacing</em>' attribute.
     * @see #getSpacing()
     * @generated
     */
    void setSpacing(int value);

} // BoxLayoutRule
