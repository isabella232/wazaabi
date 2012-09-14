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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Department</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Department#getNumber <em>Number</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Department#getEmployees <em>Employees</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getDepartment()
 * @model
 * @generated
 */
public interface Department extends EObject {
	/**
	 * Returns the value of the '<em><b>Number</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Number</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Number</em>' attribute.
	 * @see #setNumber(int)
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getDepartment_Number()
	 * @model
	 * @generated
	 */
	int getNumber();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Department#getNumber <em>Number</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Number</em>' attribute.
	 * @see #getNumber()
	 * @generated
	 */
	void setNumber(int value);

	/**
	 * Returns the value of the '<em><b>Employees</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Employees</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Employees</em>' reference list.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getDepartment_Employees()
	 * @model
	 * @generated
	 */
	EList<Employee> getEmployees();

} // Department
