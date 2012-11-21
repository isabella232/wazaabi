/**
 */
package org.eclipse.wazaabi.engine.swt.model.addressbook.impl;

import java.util.Collection;
import java.util.Date;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.wazaabi.engine.swt.model.addressbook.Address;
import org.eclipse.wazaabi.engine.swt.model.addressbook.AddressbookPackage;
import org.eclipse.wazaabi.engine.swt.model.addressbook.Person;
import org.eclipse.wazaabi.engine.swt.model.addressbook.PhoneNumber;
import org.eclipse.wazaabi.engine.swt.model.addressbook.civilState;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Person</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl#getChildren <em>Children</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl#getFirstName <em>First Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl#getLastName <em>Last Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl#getBirthDate <em>Birth Date</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl#getHomeAddress <em>Home Address</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl#getPhones <em>Phones</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.swt.model.addressbook.impl.PersonImpl#getStatus <em>Status</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PersonImpl extends EObjectImpl implements Person {
	/**
	 * The cached value of the '{@link #getChildren() <em>Children</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChildren()
	 * @generated
	 * @ordered
	 */
	protected EList<Person> children;

	/**
	 * The default value of the '{@link #getFirstName() <em>First Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFirstName()
	 * @generated
	 * @ordered
	 */
	protected static final String FIRST_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFirstName() <em>First Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFirstName()
	 * @generated
	 * @ordered
	 */
	protected String firstName = FIRST_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getLastName() <em>Last Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLastName()
	 * @generated
	 * @ordered
	 */
	protected static final String LAST_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLastName() <em>Last Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLastName()
	 * @generated
	 * @ordered
	 */
	protected String lastName = LAST_NAME_EDEFAULT;

	/**
	 * The default value of the '{@link #getBirthDate() <em>Birth Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBirthDate()
	 * @generated
	 * @ordered
	 */
	protected static final Date BIRTH_DATE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getBirthDate() <em>Birth Date</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBirthDate()
	 * @generated
	 * @ordered
	 */
	protected Date birthDate = BIRTH_DATE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getHomeAddress() <em>Home Address</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHomeAddress()
	 * @generated
	 * @ordered
	 */
	protected Address homeAddress;

	/**
	 * The cached value of the '{@link #getPhones() <em>Phones</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPhones()
	 * @generated
	 * @ordered
	 */
	protected EList<PhoneNumber> phones;

	/**
	 * The default value of the '{@link #getStatus() <em>Status</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStatus()
	 * @generated
	 * @ordered
	 */
	protected static final civilState STATUS_EDEFAULT = civilState.SINGLE;

	/**
	 * The cached value of the '{@link #getStatus() <em>Status</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStatus()
	 * @generated
	 * @ordered
	 */
	protected civilState status = STATUS_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected PersonImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return AddressbookPackage.Literals.PERSON;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Person> getChildren() {
		if (children == null) {
			children = new EObjectContainmentEList<Person>(Person.class, this, AddressbookPackage.PERSON__CHILDREN);
		}
		return children;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getFirstName() {
		return firstName;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setFirstName(String newFirstName) {
		String oldFirstName = firstName;
		firstName = newFirstName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, AddressbookPackage.PERSON__FIRST_NAME, oldFirstName, firstName));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLastName() {
		return lastName;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLastName(String newLastName) {
		String oldLastName = lastName;
		lastName = newLastName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, AddressbookPackage.PERSON__LAST_NAME, oldLastName, lastName));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Date getBirthDate() {
		return birthDate;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBirthDate(Date newBirthDate) {
		Date oldBirthDate = birthDate;
		birthDate = newBirthDate;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, AddressbookPackage.PERSON__BIRTH_DATE, oldBirthDate, birthDate));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Address getHomeAddress() {
		return homeAddress;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetHomeAddress(Address newHomeAddress, NotificationChain msgs) {
		Address oldHomeAddress = homeAddress;
		homeAddress = newHomeAddress;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, AddressbookPackage.PERSON__HOME_ADDRESS, oldHomeAddress, newHomeAddress);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setHomeAddress(Address newHomeAddress) {
		if (newHomeAddress != homeAddress) {
			NotificationChain msgs = null;
			if (homeAddress != null)
				msgs = ((InternalEObject)homeAddress).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - AddressbookPackage.PERSON__HOME_ADDRESS, null, msgs);
			if (newHomeAddress != null)
				msgs = ((InternalEObject)newHomeAddress).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - AddressbookPackage.PERSON__HOME_ADDRESS, null, msgs);
			msgs = basicSetHomeAddress(newHomeAddress, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, AddressbookPackage.PERSON__HOME_ADDRESS, newHomeAddress, newHomeAddress));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<PhoneNumber> getPhones() {
		if (phones == null) {
			phones = new EObjectContainmentEList<PhoneNumber>(PhoneNumber.class, this, AddressbookPackage.PERSON__PHONES);
		}
		return phones;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public civilState getStatus() {
		return status;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStatus(civilState newStatus) {
		civilState oldStatus = status;
		status = newStatus == null ? STATUS_EDEFAULT : newStatus;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, AddressbookPackage.PERSON__STATUS, oldStatus, status));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case AddressbookPackage.PERSON__CHILDREN:
				return ((InternalEList<?>)getChildren()).basicRemove(otherEnd, msgs);
			case AddressbookPackage.PERSON__HOME_ADDRESS:
				return basicSetHomeAddress(null, msgs);
			case AddressbookPackage.PERSON__PHONES:
				return ((InternalEList<?>)getPhones()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case AddressbookPackage.PERSON__CHILDREN:
				return getChildren();
			case AddressbookPackage.PERSON__FIRST_NAME:
				return getFirstName();
			case AddressbookPackage.PERSON__LAST_NAME:
				return getLastName();
			case AddressbookPackage.PERSON__BIRTH_DATE:
				return getBirthDate();
			case AddressbookPackage.PERSON__HOME_ADDRESS:
				return getHomeAddress();
			case AddressbookPackage.PERSON__PHONES:
				return getPhones();
			case AddressbookPackage.PERSON__STATUS:
				return getStatus();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case AddressbookPackage.PERSON__CHILDREN:
				getChildren().clear();
				getChildren().addAll((Collection<? extends Person>)newValue);
				return;
			case AddressbookPackage.PERSON__FIRST_NAME:
				setFirstName((String)newValue);
				return;
			case AddressbookPackage.PERSON__LAST_NAME:
				setLastName((String)newValue);
				return;
			case AddressbookPackage.PERSON__BIRTH_DATE:
				setBirthDate((Date)newValue);
				return;
			case AddressbookPackage.PERSON__HOME_ADDRESS:
				setHomeAddress((Address)newValue);
				return;
			case AddressbookPackage.PERSON__PHONES:
				getPhones().clear();
				getPhones().addAll((Collection<? extends PhoneNumber>)newValue);
				return;
			case AddressbookPackage.PERSON__STATUS:
				setStatus((civilState)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case AddressbookPackage.PERSON__CHILDREN:
				getChildren().clear();
				return;
			case AddressbookPackage.PERSON__FIRST_NAME:
				setFirstName(FIRST_NAME_EDEFAULT);
				return;
			case AddressbookPackage.PERSON__LAST_NAME:
				setLastName(LAST_NAME_EDEFAULT);
				return;
			case AddressbookPackage.PERSON__BIRTH_DATE:
				setBirthDate(BIRTH_DATE_EDEFAULT);
				return;
			case AddressbookPackage.PERSON__HOME_ADDRESS:
				setHomeAddress((Address)null);
				return;
			case AddressbookPackage.PERSON__PHONES:
				getPhones().clear();
				return;
			case AddressbookPackage.PERSON__STATUS:
				setStatus(STATUS_EDEFAULT);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case AddressbookPackage.PERSON__CHILDREN:
				return children != null && !children.isEmpty();
			case AddressbookPackage.PERSON__FIRST_NAME:
				return FIRST_NAME_EDEFAULT == null ? firstName != null : !FIRST_NAME_EDEFAULT.equals(firstName);
			case AddressbookPackage.PERSON__LAST_NAME:
				return LAST_NAME_EDEFAULT == null ? lastName != null : !LAST_NAME_EDEFAULT.equals(lastName);
			case AddressbookPackage.PERSON__BIRTH_DATE:
				return BIRTH_DATE_EDEFAULT == null ? birthDate != null : !BIRTH_DATE_EDEFAULT.equals(birthDate);
			case AddressbookPackage.PERSON__HOME_ADDRESS:
				return homeAddress != null;
			case AddressbookPackage.PERSON__PHONES:
				return phones != null && !phones.isEmpty();
			case AddressbookPackage.PERSON__STATUS:
				return status != STATUS_EDEFAULT;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (firstName: ");
		result.append(firstName);
		result.append(", lastName: ");
		result.append(lastName);
		result.append(", birthDate: ");
		result.append(birthDate);
		result.append(", status: ");
		result.append(status);
		result.append(')');
		return result.toString();
	}

} //PersonImpl
