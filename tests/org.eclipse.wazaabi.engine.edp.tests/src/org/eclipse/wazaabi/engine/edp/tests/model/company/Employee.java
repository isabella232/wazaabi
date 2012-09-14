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

package org.eclipse.wazaabi.engine.edp.tests.model.company;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Employee</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getFirstName <em>First Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getLastName <em>Last Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getAge <em>Age</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getEmployee()
 * @model
 * @generated
 */
public interface Employee extends EObject {
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
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getEmployee_FirstName()
	 * @model
	 * @generated
	 */
	String getFirstName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getFirstName <em>First Name</em>}' attribute.
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
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getEmployee_LastName()
	 * @model
	 * @generated
	 */
	String getLastName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getLastName <em>Last Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Last Name</em>' attribute.
	 * @see #getLastName()
	 * @generated
	 */
	void setLastName(String value);

	/**
	 * Returns the value of the '<em><b>Age</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Age</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Age</em>' attribute.
	 * @see #setAge(int)
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getEmployee_Age()
	 * @model
	 * @generated
	 */
	int getAge();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getAge <em>Age</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Age</em>' attribute.
	 * @see #getAge()
	 * @generated
	 */
	void setAge(int value);

} // Employee
