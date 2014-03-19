/**
 */
package org.eclipse.wazaabi.demo.contacts.model.contact;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Database</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.demo.contacts.model.contact.Database#getContacts <em>Contacts</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactPackage#getDatabase()
 * @model
 * @generated
 */
public interface Database extends EObject {
	/**
	 * Returns the value of the '<em><b>Contacts</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contacts</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contacts</em>' reference list.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactPackage#getDatabase_Contacts()
	 * @model
	 * @generated
	 */
	EList<Contact> getContacts();

} // Database
