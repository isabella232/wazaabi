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
 * A representation of the model object '<em><b>Expand Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#isExpanded <em>Expanded</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#getImage <em>Image</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getExpandRule()
 * @model
 * @generated
 */
public interface ExpandRule extends LayoutDataRule {
	/**
	 * Returns the value of the '<em><b>Label</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Label</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Label</em>' attribute.
	 * @see #setLabel(String)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getExpandRule_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#getLabel <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Label</em>' attribute.
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Returns the value of the '<em><b>Expanded</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Expanded</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Expanded</em>' attribute.
	 * @see #setExpanded(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getExpandRule_Expanded()
	 * @model default="false"
	 * @generated
	 */
	boolean isExpanded();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#isExpanded <em>Expanded</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Expanded</em>' attribute.
	 * @see #isExpanded()
	 * @generated
	 */
	void setExpanded(boolean value);

	/**
	 * Returns the value of the '<em><b>Image</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Image</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Image</em>' attribute.
	 * @see #setImage(String)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getExpandRule_Image()
	 * @model
	 * @generated
	 */
	String getImage();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule#getImage <em>Image</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Image</em>' attribute.
	 * @see #getImage()
	 * @generated
	 */
	void setImage(String value);

} // ExpandRule
