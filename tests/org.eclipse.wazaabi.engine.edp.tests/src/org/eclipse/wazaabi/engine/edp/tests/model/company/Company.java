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
 * A representation of the model object '<em><b>Company</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Company#getName <em>Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Company#getDepartments <em>Departments</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getCompany()
 * @model
 * @generated
 */
public interface Company extends EObject {
	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getCompany_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Company#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>Departments</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.wazaabi.engine.edp.tests.model.company.Department}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Departments</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Departments</em>' reference list.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyPackage#getCompany_Departments()
	 * @model
	 * @generated
	 */
	EList<Department> getDepartments();

} // Company
