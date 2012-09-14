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
 * A representation of the model object '<em><b>Tab Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.TabRule#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.TabRule#getImage <em>Image</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.TabRule#isClosable <em>Closable</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabRule()
 * @model
 * @generated
 */
public interface TabRule extends LayoutDataRule {
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
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabRule_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#getLabel <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Label</em>' attribute.
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

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
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabRule_Image()
	 * @model
	 * @generated
	 */
	String getImage();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#getImage <em>Image</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Image</em>' attribute.
	 * @see #getImage()
	 * @generated
	 */
	void setImage(String value);

	/**
	 * Returns the value of the '<em><b>Closable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Closable</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Closable</em>' attribute.
	 * @see #isSetClosable()
	 * @see #unsetClosable()
	 * @see #setClosable(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getTabRule_Closable()
	 * @model unsettable="true" required="true"
	 * @generated
	 */
	boolean isClosable();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#isClosable <em>Closable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Closable</em>' attribute.
	 * @see #isSetClosable()
	 * @see #unsetClosable()
	 * @see #isClosable()
	 * @generated
	 */
	void setClosable(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#isClosable <em>Closable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetClosable()
	 * @see #isClosable()
	 * @see #setClosable(boolean)
	 * @generated
	 */
	void unsetClosable();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.TabRule#isClosable <em>Closable</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Closable</em>' attribute is set.
	 * @see #unsetClosable()
	 * @see #isClosable()
	 * @see #setClosable(boolean)
	 * @generated
	 */
	boolean isSetClosable();

} // TabRule
