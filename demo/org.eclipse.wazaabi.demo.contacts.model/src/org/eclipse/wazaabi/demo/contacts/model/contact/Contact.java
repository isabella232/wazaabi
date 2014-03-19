/**
 */
package org.eclipse.wazaabi.demo.contacts.model.contact;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Contact</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getFirstName <em>First Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getLastName <em>Last Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getCompany <em>Company</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getGender <em>Gender</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactPackage#getContact()
 * @model
 * @generated
 */
public interface Contact extends EObject {
	/**
	 * Returns the value of the '<em><b>First Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>First Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>First Name</em>' attribute.
	 * @see #setFirstName(String)
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactPackage#getContact_FirstName()
	 * @model
	 * @generated
	 */
	String getFirstName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getFirstName <em>First Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>First Name</em>' attribute.
	 * @see #getFirstName()
	 * @generated
	 */
	void setFirstName(String value);

	/**
	 * Returns the value of the '<em><b>Last Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Last Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Last Name</em>' attribute.
	 * @see #setLastName(String)
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactPackage#getContact_LastName()
	 * @model
	 * @generated
	 */
	String getLastName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getLastName <em>Last Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Last Name</em>' attribute.
	 * @see #getLastName()
	 * @generated
	 */
	void setLastName(String value);

	/**
	 * Returns the value of the '<em><b>Company</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Company</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Company</em>' attribute.
	 * @see #setCompany(String)
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactPackage#getContact_Company()
	 * @model
	 * @generated
	 */
	String getCompany();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getCompany <em>Company</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Company</em>' attribute.
	 * @see #getCompany()
	 * @generated
	 */
	void setCompany(String value);

	/**
	 * Returns the value of the '<em><b>Gender</b></em>' attribute.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.demo.contacts.model.contact.Gender}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Gender</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Gender</em>' attribute.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Gender
	 * @see #setGender(Gender)
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactPackage#getContact_Gender()
	 * @model
	 * @generated
	 */
	Gender getGender();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getGender <em>Gender</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Gender</em>' attribute.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Gender
	 * @see #getGender()
	 * @generated
	 */
	void setGender(Gender value);

} // Contact
