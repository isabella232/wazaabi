/**
 */
package org.eclipse.wazaabi.engine.swt.model.addressbook;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Phone Number</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getType <em>Type</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getNumber <em>Number</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPhoneNumber()
 * @model
 * @generated
 */
public interface PhoneNumber extends EObject {
	/**
	 * Returns the value of the '<em><b>Type</b></em>' attribute.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' attribute.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType
	 * @see #setType(PhoneType)
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPhoneNumber_Type()
	 * @model
	 * @generated
	 */
	PhoneType getType();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getType <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' attribute.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType
	 * @see #getType()
	 * @generated
	 */
	void setType(PhoneType value);

	/**
	 * Returns the value of the '<em><b>Number</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Number</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Number</em>' attribute.
	 * @see #setNumber(String)
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPhoneNumber_Number()
	 * @model
	 * @generated
	 */
	String getNumber();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getNumber <em>Number</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Number</em>' attribute.
	 * @see #getNumber()
	 * @generated
	 */
	void setNumber(String value);

} // PhoneNumber
