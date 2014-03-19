/**
 */
package org.eclipse.wazaabi.demo.contacts.model.contact;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.demo.contacts.model.contact.ContactFactory
 * @model kind="package"
 * @generated
 */
public interface ContactPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "contact";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "urn:contact";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "cnt";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	ContactPackage eINSTANCE = org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactImpl <em>Contact</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactImpl
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactPackageImpl#getContact()
	 * @generated
	 */
	int CONTACT = 0;

	/**
	 * The feature id for the '<em><b>First Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTACT__FIRST_NAME = 0;

	/**
	 * The feature id for the '<em><b>Last Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTACT__LAST_NAME = 1;

	/**
	 * The feature id for the '<em><b>Company</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTACT__COMPANY = 2;

	/**
	 * The feature id for the '<em><b>Gender</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTACT__GENDER = 3;

	/**
	 * The number of structural features of the '<em>Contact</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTACT_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>Contact</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTACT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.impl.DatabaseImpl <em>Database</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.DatabaseImpl
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactPackageImpl#getDatabase()
	 * @generated
	 */
	int DATABASE = 1;

	/**
	 * The feature id for the '<em><b>Contacts</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATABASE__CONTACTS = 0;

	/**
	 * The number of structural features of the '<em>Database</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATABASE_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Database</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATABASE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Gender <em>Gender</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Gender
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactPackageImpl#getGender()
	 * @generated
	 */
	int GENDER = 2;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact <em>Contact</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Contact</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Contact
	 * @generated
	 */
	EClass getContact();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getFirstName <em>First Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>First Name</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getFirstName()
	 * @see #getContact()
	 * @generated
	 */
	EAttribute getContact_FirstName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getLastName <em>Last Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Last Name</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getLastName()
	 * @see #getContact()
	 * @generated
	 */
	EAttribute getContact_LastName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getCompany <em>Company</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Company</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getCompany()
	 * @see #getContact()
	 * @generated
	 */
	EAttribute getContact_Company();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getGender <em>Gender</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Gender</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Contact#getGender()
	 * @see #getContact()
	 * @generated
	 */
	EAttribute getContact_Gender();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Database <em>Database</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Database</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Database
	 * @generated
	 */
	EClass getDatabase();

	/**
	 * Returns the meta object for the reference list '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Database#getContacts <em>Contacts</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Contacts</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Database#getContacts()
	 * @see #getDatabase()
	 * @generated
	 */
	EReference getDatabase_Contacts();

	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Gender <em>Gender</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Gender</em>'.
	 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Gender
	 * @generated
	 */
	EEnum getGender();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	ContactFactory getContactFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactImpl <em>Contact</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactImpl
		 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactPackageImpl#getContact()
		 * @generated
		 */
		EClass CONTACT = eINSTANCE.getContact();

		/**
		 * The meta object literal for the '<em><b>First Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONTACT__FIRST_NAME = eINSTANCE.getContact_FirstName();

		/**
		 * The meta object literal for the '<em><b>Last Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONTACT__LAST_NAME = eINSTANCE.getContact_LastName();

		/**
		 * The meta object literal for the '<em><b>Company</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONTACT__COMPANY = eINSTANCE.getContact_Company();

		/**
		 * The meta object literal for the '<em><b>Gender</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONTACT__GENDER = eINSTANCE.getContact_Gender();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.impl.DatabaseImpl <em>Database</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.DatabaseImpl
		 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactPackageImpl#getDatabase()
		 * @generated
		 */
		EClass DATABASE = eINSTANCE.getDatabase();

		/**
		 * The meta object literal for the '<em><b>Contacts</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATABASE__CONTACTS = eINSTANCE.getDatabase_Contacts();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.demo.contacts.model.contact.Gender <em>Gender</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.demo.contacts.model.contact.Gender
		 * @see org.eclipse.wazaabi.demo.contacts.model.contact.impl.ContactPackageImpl#getGender()
		 * @generated
		 */
		EEnum GENDER = eINSTANCE.getGender();

	}

} //ContactPackage
