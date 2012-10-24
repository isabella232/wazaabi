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
 * A representation of the model object '<em><b>Form Data Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getBottom <em>Bottom</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getLeft <em>Left</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getRight <em>Right</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getTop <em>Top</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getHeight <em>Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getWidth <em>Width</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFormDataRule()
 * @model
 * @generated
 */
public interface FormDataRule extends LayoutDataRule {
	/**
	 * Returns the value of the '<em><b>Bottom</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bottom</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Bottom</em>' containment reference.
	 * @see #setBottom(FormAttachment)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFormDataRule_Bottom()
	 * @model containment="true"
	 * @generated
	 */
	FormAttachment getBottom();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getBottom <em>Bottom</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Bottom</em>' containment reference.
	 * @see #getBottom()
	 * @generated
	 */
	void setBottom(FormAttachment value);

	/**
	 * Returns the value of the '<em><b>Left</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Left</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Left</em>' containment reference.
	 * @see #setLeft(FormAttachment)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFormDataRule_Left()
	 * @model containment="true"
	 * @generated
	 */
	FormAttachment getLeft();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getLeft <em>Left</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Left</em>' containment reference.
	 * @see #getLeft()
	 * @generated
	 */
	void setLeft(FormAttachment value);

	/**
	 * Returns the value of the '<em><b>Right</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Right</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Right</em>' containment reference.
	 * @see #setRight(FormAttachment)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFormDataRule_Right()
	 * @model containment="true"
	 * @generated
	 */
	FormAttachment getRight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getRight <em>Right</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Right</em>' containment reference.
	 * @see #getRight()
	 * @generated
	 */
	void setRight(FormAttachment value);

	/**
	 * Returns the value of the '<em><b>Top</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Top</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Top</em>' containment reference.
	 * @see #setTop(FormAttachment)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFormDataRule_Top()
	 * @model containment="true"
	 * @generated
	 */
	FormAttachment getTop();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getTop <em>Top</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Top</em>' containment reference.
	 * @see #getTop()
	 * @generated
	 */
	void setTop(FormAttachment value);

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
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFormDataRule_Height()
	 * @model default="-1"
	 * @generated
	 */
	int getHeight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getHeight <em>Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Height</em>' attribute.
	 * @see #getHeight()
	 * @generated
	 */
	void setHeight(int value);

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
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getFormDataRule_Width()
	 * @model default="-1"
	 * @generated
	 */
	int getWidth();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.FormDataRule#getWidth <em>Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Width</em>' attribute.
	 * @see #getWidth()
	 * @generated
	 */
	void setWidth(int value);

} // FormDataRule
