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
 * A representation of the model object '<em><b>Font Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getName <em>Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getHeight <em>Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isItalic <em>Italic</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isBold <em>Bold</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getFontRule()
 * @model
 * @generated
 */
public interface FontRule extends StyleRule {
	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #isSetName()
	 * @see #unsetName()
	 * @see #setName(String)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getFontRule_Name()
	 * @model unsettable="true"
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #isSetName()
	 * @see #unsetName()
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetName()
	 * @see #getName()
	 * @see #setName(String)
	 * @generated
	 */
	void unsetName();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getName <em>Name</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Name</em>' attribute is set.
	 * @see #unsetName()
	 * @see #getName()
	 * @see #setName(String)
	 * @generated
	 */
	boolean isSetName();

	/**
	 * Returns the value of the '<em><b>Height</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Height</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Height</em>' attribute.
	 * @see #isSetHeight()
	 * @see #unsetHeight()
	 * @see #setHeight(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getFontRule_Height()
	 * @model unsettable="true"
	 * @generated
	 */
	int getHeight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getHeight <em>Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Height</em>' attribute.
	 * @see #isSetHeight()
	 * @see #unsetHeight()
	 * @see #getHeight()
	 * @generated
	 */
	void setHeight(int value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getHeight <em>Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetHeight()
	 * @see #getHeight()
	 * @see #setHeight(int)
	 * @generated
	 */
	void unsetHeight();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#getHeight <em>Height</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Height</em>' attribute is set.
	 * @see #unsetHeight()
	 * @see #getHeight()
	 * @see #setHeight(int)
	 * @generated
	 */
	boolean isSetHeight();

	/**
	 * Returns the value of the '<em><b>Italic</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Italic</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Italic</em>' attribute.
	 * @see #isSetItalic()
	 * @see #unsetItalic()
	 * @see #setItalic(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getFontRule_Italic()
	 * @model unsettable="true"
	 * @generated
	 */
	boolean isItalic();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isItalic <em>Italic</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Italic</em>' attribute.
	 * @see #isSetItalic()
	 * @see #unsetItalic()
	 * @see #isItalic()
	 * @generated
	 */
	void setItalic(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isItalic <em>Italic</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetItalic()
	 * @see #isItalic()
	 * @see #setItalic(boolean)
	 * @generated
	 */
	void unsetItalic();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isItalic <em>Italic</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Italic</em>' attribute is set.
	 * @see #unsetItalic()
	 * @see #isItalic()
	 * @see #setItalic(boolean)
	 * @generated
	 */
	boolean isSetItalic();

	/**
	 * Returns the value of the '<em><b>Bold</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bold</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Bold</em>' attribute.
	 * @see #isSetBold()
	 * @see #unsetBold()
	 * @see #setBold(boolean)
	 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getFontRule_Bold()
	 * @model unsettable="true"
	 * @generated
	 */
	boolean isBold();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isBold <em>Bold</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Bold</em>' attribute.
	 * @see #isSetBold()
	 * @see #unsetBold()
	 * @see #isBold()
	 * @generated
	 */
	void setBold(boolean value);

	/**
	 * Unsets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isBold <em>Bold</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetBold()
	 * @see #isBold()
	 * @see #setBold(boolean)
	 * @generated
	 */
	void unsetBold();

	/**
	 * Returns whether the value of the '{@link org.eclipse.wazaabi.mm.core.styles.FontRule#isBold <em>Bold</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Bold</em>' attribute is set.
	 * @see #unsetBold()
	 * @see #isBold()
	 * @see #setBold(boolean)
	 * @generated
	 */
	boolean isSetBold();

} // FontRule
