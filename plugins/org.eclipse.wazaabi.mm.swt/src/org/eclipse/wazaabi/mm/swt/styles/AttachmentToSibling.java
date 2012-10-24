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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Attachment To Sibling</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getSiblingId <em>Sibling Id</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getAlignment <em>Alignment</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getAttachmentToSibling()
 * @model
 * @generated
 */
public interface AttachmentToSibling extends FormAttachment {
	/**
	 * Returns the value of the '<em><b>Sibling Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Sibling Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sibling Id</em>' attribute.
	 * @see #setSiblingId(String)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getAttachmentToSibling_SiblingId()
	 * @model
	 * @generated
	 */
	String getSiblingId();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getSiblingId <em>Sibling Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Sibling Id</em>' attribute.
	 * @see #getSiblingId()
	 * @generated
	 */
	void setSiblingId(String value);

	/**
	 * Returns the value of the '<em><b>Alignment</b></em>' attribute.
	 * The default value is <code>"DEFAULT"</code>.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Alignment</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Alignment</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment
	 * @see #setAlignment(ToSiblingAlignment)
	 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage#getAttachmentToSibling_Alignment()
	 * @model default="DEFAULT"
	 * @generated
	 */
	ToSiblingAlignment getAlignment();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling#getAlignment <em>Alignment</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Alignment</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment
	 * @see #getAlignment()
	 * @generated
	 */
	void setAlignment(ToSiblingAlignment value);

} // AttachmentToSibling
