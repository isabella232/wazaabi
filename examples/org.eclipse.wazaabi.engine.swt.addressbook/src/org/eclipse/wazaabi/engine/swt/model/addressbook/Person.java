/**
 */
package org.eclipse.wazaabi.engine.swt.model.addressbook;

import java.util.Date;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Person</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getChildren <em>Children</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getFirstName <em>First Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getLastName <em>Last Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getBirthDate <em>Birth Date</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getHomeAddress <em>Home Address</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getPhones <em>Phones</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getStatus <em>Status</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson()
 * @model
 * @generated
 */
public interface Person extends EObject {
	/**
	 * Returns the value of the '<em><b>Children</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Children</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Children</em>' containment reference list.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson_Children()
	 * @model containment="true"
	 * @generated
	 */
	EList<Person> getChildren();

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
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson_FirstName()
	 * @model
	 * @generated
	 */
	String getFirstName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getFirstName <em>First Name</em>}' attribute.
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
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson_LastName()
	 * @model
	 * @generated
	 */
	String getLastName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getLastName <em>Last Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Last Name</em>' attribute.
	 * @see #getLastName()
	 * @generated
	 */
	void setLastName(String value);

	/**
	 * Returns the value of the '<em><b>Birth Date</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Birth Date</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Birth Date</em>' attribute.
	 * @see #setBirthDate(Date)
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson_BirthDate()
	 * @model
	 * @generated
	 */
	Date getBirthDate();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getBirthDate <em>Birth Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Birth Date</em>' attribute.
	 * @see #getBirthDate()
	 * @generated
	 */
	void setBirthDate(Date value);

	/**
	 * Returns the value of the '<em><b>Home Address</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Home Address</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Home Address</em>' containment reference.
	 * @see #setHomeAddress(Address)
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson_HomeAddress()
	 * @model containment="true"
	 * @generated
	 */
	Address getHomeAddress();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getHomeAddress <em>Home Address</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Home Address</em>' containment reference.
	 * @see #getHomeAddress()
	 * @generated
	 */
	void setHomeAddress(Address value);

	/**
	 * Returns the value of the '<em><b>Phones</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Phones</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Phones</em>' containment reference list.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson_Phones()
	 * @model containment="true"
	 * @generated
	 */
	EList<PhoneNumber> getPhones();

	/**
	 * Returns the value of the '<em><b>Status</b></em>' attribute.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.engine.swt.model.addressbook.civilState}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Status</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Status</em>' attribute.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.civilState
	 * @see #setStatus(civilState)
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage#getPerson_Status()
	 * @model
	 * @generated
	 */
	civilState getStatus();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getStatus <em>Status</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Status</em>' attribute.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.civilState
	 * @see #getStatus()
	 * @generated
	 */
	void setStatus(civilState value);

} // Person
