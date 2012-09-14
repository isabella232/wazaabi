/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.edp.tests.model.company.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage;
import org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Test Class</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl#getStringAttribute1 <em>String Attribute1</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl#getStringAttribute2 <em>String Attribute2</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl#getIntAttribute1 <em>Int Attribute1</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl#getIntAttribute2 <em>Int Attribute2</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TestClassImpl extends EObjectImpl implements TestClass {
	/**
	 * The default value of the '{@link #getStringAttribute1() <em>String Attribute1</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStringAttribute1()
	 * @generated
	 * @ordered
	 */
	protected static final String STRING_ATTRIBUTE1_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getStringAttribute1() <em>String Attribute1</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStringAttribute1()
	 * @generated
	 * @ordered
	 */
	protected String stringAttribute1 = STRING_ATTRIBUTE1_EDEFAULT;

	/**
	 * The default value of the '{@link #getStringAttribute2() <em>String Attribute2</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStringAttribute2()
	 * @generated
	 * @ordered
	 */
	protected static final String STRING_ATTRIBUTE2_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getStringAttribute2() <em>String Attribute2</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStringAttribute2()
	 * @generated
	 * @ordered
	 */
	protected String stringAttribute2 = STRING_ATTRIBUTE2_EDEFAULT;

	/**
	 * The default value of the '{@link #getIntAttribute1() <em>Int Attribute1</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntAttribute1()
	 * @generated
	 * @ordered
	 */
	protected static final int INT_ATTRIBUTE1_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getIntAttribute1() <em>Int Attribute1</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntAttribute1()
	 * @generated
	 * @ordered
	 */
	protected int intAttribute1 = INT_ATTRIBUTE1_EDEFAULT;

	/**
	 * The default value of the '{@link #getIntAttribute2() <em>Int Attribute2</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntAttribute2()
	 * @generated
	 * @ordered
	 */
	protected static final int INT_ATTRIBUTE2_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getIntAttribute2() <em>Int Attribute2</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntAttribute2()
	 * @generated
	 * @ordered
	 */
	protected int intAttribute2 = INT_ATTRIBUTE2_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TestClassImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CompanyPackage.Literals.TEST_CLASS;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getStringAttribute1() {
		return stringAttribute1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStringAttribute1(String newStringAttribute1) {
		String oldStringAttribute1 = stringAttribute1;
		stringAttribute1 = newStringAttribute1;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE1, oldStringAttribute1, stringAttribute1));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getStringAttribute2() {
		return stringAttribute2;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStringAttribute2(String newStringAttribute2) {
		String oldStringAttribute2 = stringAttribute2;
		stringAttribute2 = newStringAttribute2;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE2, oldStringAttribute2, stringAttribute2));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getIntAttribute1() {
		return intAttribute1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIntAttribute1(int newIntAttribute1) {
		int oldIntAttribute1 = intAttribute1;
		intAttribute1 = newIntAttribute1;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CompanyPackage.TEST_CLASS__INT_ATTRIBUTE1, oldIntAttribute1, intAttribute1));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getIntAttribute2() {
		return intAttribute2;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIntAttribute2(int newIntAttribute2) {
		int oldIntAttribute2 = intAttribute2;
		intAttribute2 = newIntAttribute2;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CompanyPackage.TEST_CLASS__INT_ATTRIBUTE2, oldIntAttribute2, intAttribute2));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE1:
				return getStringAttribute1();
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE2:
				return getStringAttribute2();
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE1:
				return getIntAttribute1();
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE2:
				return getIntAttribute2();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE1:
				setStringAttribute1((String)newValue);
				return;
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE2:
				setStringAttribute2((String)newValue);
				return;
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE1:
				setIntAttribute1((Integer)newValue);
				return;
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE2:
				setIntAttribute2((Integer)newValue);
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
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE1:
				setStringAttribute1(STRING_ATTRIBUTE1_EDEFAULT);
				return;
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE2:
				setStringAttribute2(STRING_ATTRIBUTE2_EDEFAULT);
				return;
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE1:
				setIntAttribute1(INT_ATTRIBUTE1_EDEFAULT);
				return;
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE2:
				setIntAttribute2(INT_ATTRIBUTE2_EDEFAULT);
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
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE1:
				return STRING_ATTRIBUTE1_EDEFAULT == null ? stringAttribute1 != null : !STRING_ATTRIBUTE1_EDEFAULT.equals(stringAttribute1);
			case CompanyPackage.TEST_CLASS__STRING_ATTRIBUTE2:
				return STRING_ATTRIBUTE2_EDEFAULT == null ? stringAttribute2 != null : !STRING_ATTRIBUTE2_EDEFAULT.equals(stringAttribute2);
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE1:
				return intAttribute1 != INT_ATTRIBUTE1_EDEFAULT;
			case CompanyPackage.TEST_CLASS__INT_ATTRIBUTE2:
				return intAttribute2 != INT_ATTRIBUTE2_EDEFAULT;
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
		result.append(" (stringAttribute1: ");
		result.append(stringAttribute1);
		result.append(", stringAttribute2: ");
		result.append(stringAttribute2);
		result.append(", intAttribute1: ");
		result.append(intAttribute1);
		result.append(", intAttribute2: ");
		result.append(intAttribute2);
		result.append(')');
		return result.toString();
	}

} //TestClassImpl
