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

import org.eclipse.wazaabi.mm.core.Position;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Tabbed Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMaximizeVisible <em>Maximize Visible</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMinimizeVisible <em>Minimize Visible</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#getPosition <em>Position</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabbedLayoutRule()
 * @model
 * @generated
 */
public interface TabbedLayoutRule extends StackLayoutRule {
	/**
	 * Returns the value of the '<em><b>Maximize Visible</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Maximize Visible</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Maximize Visible</em>' attribute.
	 * @see #isSetMaximizeVisible()
	 * @see #unsetMaximizeVisible()
	 * @see #setMaximizeVisible(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabbedLayoutRule_MaximizeVisible()
	 * @model unsettable="true" required="true"
	 * @generated
	 */
	boolean isMaximizeVisible();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMaximizeVisible <em>Maximize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Maximize Visible</em>' attribute.
	 * @see #isSetMaximizeVisible()
	 * @see #unsetMaximizeVisible()
	 * @see #isMaximizeVisible()
	 * @generated
	 */
	void setMaximizeVisible(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMaximizeVisible <em>Maximize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetMaximizeVisible()
	 * @see #isMaximizeVisible()
	 * @see #setMaximizeVisible(boolean)
	 * @generated
	 */
	void unsetMaximizeVisible();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMaximizeVisible <em>Maximize Visible</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Maximize Visible</em>' attribute is set.
	 * @see #unsetMaximizeVisible()
	 * @see #isMaximizeVisible()
	 * @see #setMaximizeVisible(boolean)
	 * @generated
	 */
	boolean isSetMaximizeVisible();

	/**
	 * Returns the value of the '<em><b>Minimize Visible</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Minimize Visible</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Minimize Visible</em>' attribute.
	 * @see #isSetMinimizeVisible()
	 * @see #unsetMinimizeVisible()
	 * @see #setMinimizeVisible(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabbedLayoutRule_MinimizeVisible()
	 * @model unsettable="true" required="true"
	 * @generated
	 */
	boolean isMinimizeVisible();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMinimizeVisible <em>Minimize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Minimize Visible</em>' attribute.
	 * @see #isSetMinimizeVisible()
	 * @see #unsetMinimizeVisible()
	 * @see #isMinimizeVisible()
	 * @generated
	 */
	void setMinimizeVisible(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMinimizeVisible <em>Minimize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetMinimizeVisible()
	 * @see #isMinimizeVisible()
	 * @see #setMinimizeVisible(boolean)
	 * @generated
	 */
	void unsetMinimizeVisible();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#isMinimizeVisible <em>Minimize Visible</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Minimize Visible</em>' attribute is set.
	 * @see #unsetMinimizeVisible()
	 * @see #isMinimizeVisible()
	 * @see #setMinimizeVisible(boolean)
	 * @generated
	 */
	boolean isSetMinimizeVisible();

	/**
	 * Returns the value of the '<em><b>Position</b></em>' attribute.
	 * The default value is <code>"TOP"</code>.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.core.Position}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Position</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Position</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Position
	 * @see #setPosition(Position)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabbedLayoutRule_Position()
	 * @model default="TOP"
	 * @generated
	 */
	Position getPosition();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule#getPosition <em>Position</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Position</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.Position
	 * @see #getPosition()
	 * @generated
	 */
	void setPosition(Position value);

} // TabbedLayoutRule
