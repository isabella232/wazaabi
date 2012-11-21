/**
 */
package org.eclipse.wazaabi.engine.swt.model.addressbook;

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
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookFactory
 * @model kind="package"
 * @generated
 */
public interface AddressbookPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "addressbook";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/addressbook";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "ab";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	AddressbookPackage eINSTANCE = org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressBookImpl <em>Address Book</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressBookImpl
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getAddressBook()
	 * @generated
	 */
	int ADDRESS_BOOK = 0;

	/**
	 * The feature id for the '<em><b>Persons</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS_BOOK__PERSONS = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS_BOOK__NAME = 1;

	/**
	 * The number of structural features of the '<em>Address Book</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS_BOOK_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl <em>Person</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getPerson()
	 * @generated
	 */
	int PERSON = 1;

	/**
	 * The feature id for the '<em><b>Children</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON__CHILDREN = 0;

	/**
	 * The feature id for the '<em><b>First Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON__FIRST_NAME = 1;

	/**
	 * The feature id for the '<em><b>Last Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON__LAST_NAME = 2;

	/**
	 * The feature id for the '<em><b>Birth Date</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON__BIRTH_DATE = 3;

	/**
	 * The feature id for the '<em><b>Home Address</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON__HOME_ADDRESS = 4;

	/**
	 * The feature id for the '<em><b>Phones</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON__PHONES = 5;

	/**
	 * The feature id for the '<em><b>Status</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON__STATUS = 6;

	/**
	 * The number of structural features of the '<em>Person</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERSON_FEATURE_COUNT = 7;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressImpl <em>Address</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressImpl
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getAddress()
	 * @generated
	 */
	int ADDRESS = 2;

	/**
	 * The feature id for the '<em><b>Street</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS__STREET = 0;

	/**
	 * The feature id for the '<em><b>Number</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS__NUMBER = 1;

	/**
	 * The feature id for the '<em><b>Postal Code</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS__POSTAL_CODE = 2;

	/**
	 * The feature id for the '<em><b>City</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS__CITY = 3;

	/**
	 * The feature id for the '<em><b>Country</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS__COUNTRY = 4;

	/**
	 * The number of structural features of the '<em>Address</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ADDRESS_FEATURE_COUNT = 5;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PhoneNumberImpl <em>Phone Number</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PhoneNumberImpl
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getPhoneNumber()
	 * @generated
	 */
	int PHONE_NUMBER = 3;

	/**
	 * The feature id for the '<em><b>Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PHONE_NUMBER__TYPE = 0;

	/**
	 * The feature id for the '<em><b>Number</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PHONE_NUMBER__NUMBER = 1;

	/**
	 * The number of structural features of the '<em>Phone Number</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PHONE_NUMBER_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.civilState <em>civil State</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.civilState
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getcivilState()
	 * @generated
	 */
	int CIVIL_STATE = 4;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType <em>Phone Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getPhoneType()
	 * @generated
	 */
	int PHONE_TYPE = 5;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.AddressBook <em>Address Book</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Address Book</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressBook
	 * @generated
	 */
	EClass getAddressBook();

	/**
	 * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.AddressBook#getPersons <em>Persons</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Persons</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressBook#getPersons()
	 * @see #getAddressBook()
	 * @generated
	 */
	EReference getAddressBook_Persons();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.AddressBook#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.AddressBook#getName()
	 * @see #getAddressBook()
	 * @generated
	 */
	EAttribute getAddressBook_Name();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person <em>Person</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Person</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person
	 * @generated
	 */
	EClass getPerson();

	/**
	 * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getChildren <em>Children</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Children</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getChildren()
	 * @see #getPerson()
	 * @generated
	 */
	EReference getPerson_Children();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getFirstName <em>First Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>First Name</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getFirstName()
	 * @see #getPerson()
	 * @generated
	 */
	EAttribute getPerson_FirstName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getLastName <em>Last Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Last Name</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getLastName()
	 * @see #getPerson()
	 * @generated
	 */
	EAttribute getPerson_LastName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getBirthDate <em>Birth Date</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Birth Date</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getBirthDate()
	 * @see #getPerson()
	 * @generated
	 */
	EAttribute getPerson_BirthDate();

	/**
	 * Returns the meta object for the containment reference '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getHomeAddress <em>Home Address</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Home Address</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getHomeAddress()
	 * @see #getPerson()
	 * @generated
	 */
	EReference getPerson_HomeAddress();

	/**
	 * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getPhones <em>Phones</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Phones</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getPhones()
	 * @see #getPerson()
	 * @generated
	 */
	EReference getPerson_Phones();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getStatus <em>Status</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Status</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Person#getStatus()
	 * @see #getPerson()
	 * @generated
	 */
	EAttribute getPerson_Status();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Address <em>Address</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Address</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Address
	 * @generated
	 */
	EClass getAddress();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getStreet <em>Street</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Street</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getStreet()
	 * @see #getAddress()
	 * @generated
	 */
	EAttribute getAddress_Street();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getNumber <em>Number</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Number</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getNumber()
	 * @see #getAddress()
	 * @generated
	 */
	EAttribute getAddress_Number();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getPostalCode <em>Postal Code</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Postal Code</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getPostalCode()
	 * @see #getAddress()
	 * @generated
	 */
	EAttribute getAddress_PostalCode();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getCity <em>City</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>City</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getCity()
	 * @see #getAddress()
	 * @generated
	 */
	EAttribute getAddress_City();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getCountry <em>Country</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Country</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.Address#getCountry()
	 * @see #getAddress()
	 * @generated
	 */
	EAttribute getAddress_Country();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber <em>Phone Number</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Phone Number</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber
	 * @generated
	 */
	EClass getPhoneNumber();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Type</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getType()
	 * @see #getPhoneNumber()
	 * @generated
	 */
	EAttribute getPhoneNumber_Type();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getNumber <em>Number</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Number</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber#getNumber()
	 * @see #getPhoneNumber()
	 * @generated
	 */
	EAttribute getPhoneNumber_Number();

	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.civilState <em>civil State</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>civil State</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.civilState
	 * @generated
	 */
	EEnum getcivilState();

	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType <em>Phone Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Phone Type</em>'.
	 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType
	 * @generated
	 */
	EEnum getPhoneType();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	AddressbookFactory getAddressbookFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressBookImpl <em>Address Book</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressBookImpl
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getAddressBook()
		 * @generated
		 */
		EClass ADDRESS_BOOK = eINSTANCE.getAddressBook();

		/**
		 * The meta object literal for the '<em><b>Persons</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ADDRESS_BOOK__PERSONS = eINSTANCE.getAddressBook_Persons();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ADDRESS_BOOK__NAME = eINSTANCE.getAddressBook_Name();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl <em>Person</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getPerson()
		 * @generated
		 */
		EClass PERSON = eINSTANCE.getPerson();

		/**
		 * The meta object literal for the '<em><b>Children</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PERSON__CHILDREN = eINSTANCE.getPerson_Children();

		/**
		 * The meta object literal for the '<em><b>First Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PERSON__FIRST_NAME = eINSTANCE.getPerson_FirstName();

		/**
		 * The meta object literal for the '<em><b>Last Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PERSON__LAST_NAME = eINSTANCE.getPerson_LastName();

		/**
		 * The meta object literal for the '<em><b>Birth Date</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PERSON__BIRTH_DATE = eINSTANCE.getPerson_BirthDate();

		/**
		 * The meta object literal for the '<em><b>Home Address</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PERSON__HOME_ADDRESS = eINSTANCE.getPerson_HomeAddress();

		/**
		 * The meta object literal for the '<em><b>Phones</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PERSON__PHONES = eINSTANCE.getPerson_Phones();

		/**
		 * The meta object literal for the '<em><b>Status</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PERSON__STATUS = eINSTANCE.getPerson_Status();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressImpl <em>Address</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressImpl
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getAddress()
		 * @generated
		 */
		EClass ADDRESS = eINSTANCE.getAddress();

		/**
		 * The meta object literal for the '<em><b>Street</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ADDRESS__STREET = eINSTANCE.getAddress_Street();

		/**
		 * The meta object literal for the '<em><b>Number</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ADDRESS__NUMBER = eINSTANCE.getAddress_Number();

		/**
		 * The meta object literal for the '<em><b>Postal Code</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ADDRESS__POSTAL_CODE = eINSTANCE.getAddress_PostalCode();

		/**
		 * The meta object literal for the '<em><b>City</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ADDRESS__CITY = eINSTANCE.getAddress_City();

		/**
		 * The meta object literal for the '<em><b>Country</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ADDRESS__COUNTRY = eINSTANCE.getAddress_Country();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PhoneNumberImpl <em>Phone Number</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PhoneNumberImpl
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getPhoneNumber()
		 * @generated
		 */
		EClass PHONE_NUMBER = eINSTANCE.getPhoneNumber();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PHONE_NUMBER__TYPE = eINSTANCE.getPhoneNumber_Type();

		/**
		 * The meta object literal for the '<em><b>Number</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PHONE_NUMBER__NUMBER = eINSTANCE.getPhoneNumber_Number();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.civilState <em>civil State</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.civilState
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getcivilState()
		 * @generated
		 */
		EEnum CIVIL_STATE = eINSTANCE.getcivilState();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType <em>Phone Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneType
		 * @see org.eclipse.wazaabi.engine.swt.model.addressbook.impl.AddressbookPackageImpl#getPhoneType()
		 * @generated
		 */
		EEnum PHONE_TYPE = eINSTANCE.getPhoneType();

	}

} //AddressbookPackage
