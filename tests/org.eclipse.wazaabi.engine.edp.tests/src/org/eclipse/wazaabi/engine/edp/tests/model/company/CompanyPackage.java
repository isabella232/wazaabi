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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
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
 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.CompanyFactory
 * @model kind="package"
 * @generated
 */
public interface CompanyPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "company";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://com.example.company.ecore";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "company";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CompanyPackage eINSTANCE = org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.DepartmentImpl <em>Department</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.DepartmentImpl
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getDepartment()
	 * @generated
	 */
	int DEPARTMENT = 0;

	/**
	 * The feature id for the '<em><b>Number</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DEPARTMENT__NUMBER = 0;

	/**
	 * The feature id for the '<em><b>Employees</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DEPARTMENT__EMPLOYEES = 1;

	/**
	 * The number of structural features of the '<em>Department</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DEPARTMENT_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.EmployeeImpl <em>Employee</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.EmployeeImpl
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getEmployee()
	 * @generated
	 */
	int EMPLOYEE = 1;

	/**
	 * The feature id for the '<em><b>First Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EMPLOYEE__FIRST_NAME = 0;

	/**
	 * The feature id for the '<em><b>Last Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EMPLOYEE__LAST_NAME = 1;

	/**
	 * The feature id for the '<em><b>Age</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EMPLOYEE__AGE = 2;

	/**
	 * The number of structural features of the '<em>Employee</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EMPLOYEE_FEATURE_COUNT = 3;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyImpl <em>Company</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyImpl
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getCompany()
	 * @generated
	 */
	int COMPANY = 2;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPANY__NAME = 0;

	/**
	 * The feature id for the '<em><b>Departments</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPANY__DEPARTMENTS = 1;

	/**
	 * The number of structural features of the '<em>Company</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPANY_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl <em>Test Class</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getTestClass()
	 * @generated
	 */
	int TEST_CLASS = 3;

	/**
	 * The feature id for the '<em><b>String Attribute1</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_CLASS__STRING_ATTRIBUTE1 = 0;

	/**
	 * The feature id for the '<em><b>String Attribute2</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_CLASS__STRING_ATTRIBUTE2 = 1;

	/**
	 * The feature id for the '<em><b>Int Attribute1</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_CLASS__INT_ATTRIBUTE1 = 2;

	/**
	 * The feature id for the '<em><b>Int Attribute2</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_CLASS__INT_ATTRIBUTE2 = 3;

	/**
	 * The number of structural features of the '<em>Test Class</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TEST_CLASS_FEATURE_COUNT = 4;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Department <em>Department</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Department</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Department
	 * @generated
	 */
	EClass getDepartment();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Department#getNumber <em>Number</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Number</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Department#getNumber()
	 * @see #getDepartment()
	 * @generated
	 */
	EAttribute getDepartment_Number();

	/**
	 * Returns the meta object for the reference list '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Department#getEmployees <em>Employees</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Employees</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Department#getEmployees()
	 * @see #getDepartment()
	 * @generated
	 */
	EReference getDepartment_Employees();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee <em>Employee</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Employee</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Employee
	 * @generated
	 */
	EClass getEmployee();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getFirstName <em>First Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>First Name</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getFirstName()
	 * @see #getEmployee()
	 * @generated
	 */
	EAttribute getEmployee_FirstName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getLastName <em>Last Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Last Name</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getLastName()
	 * @see #getEmployee()
	 * @generated
	 */
	EAttribute getEmployee_LastName();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getAge <em>Age</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Age</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Employee#getAge()
	 * @see #getEmployee()
	 * @generated
	 */
	EAttribute getEmployee_Age();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Company <em>Company</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Company</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Company
	 * @generated
	 */
	EClass getCompany();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Company#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Company#getName()
	 * @see #getCompany()
	 * @generated
	 */
	EAttribute getCompany_Name();

	/**
	 * Returns the meta object for the reference list '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.Company#getDepartments <em>Departments</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Departments</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.Company#getDepartments()
	 * @see #getCompany()
	 * @generated
	 */
	EReference getCompany_Departments();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass <em>Test Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Test Class</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass
	 * @generated
	 */
	EClass getTestClass();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getStringAttribute1 <em>String Attribute1</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>String Attribute1</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getStringAttribute1()
	 * @see #getTestClass()
	 * @generated
	 */
	EAttribute getTestClass_StringAttribute1();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getStringAttribute2 <em>String Attribute2</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>String Attribute2</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getStringAttribute2()
	 * @see #getTestClass()
	 * @generated
	 */
	EAttribute getTestClass_StringAttribute2();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getIntAttribute1 <em>Int Attribute1</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Int Attribute1</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getIntAttribute1()
	 * @see #getTestClass()
	 * @generated
	 */
	EAttribute getTestClass_IntAttribute1();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getIntAttribute2 <em>Int Attribute2</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Int Attribute2</em>'.
	 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.TestClass#getIntAttribute2()
	 * @see #getTestClass()
	 * @generated
	 */
	EAttribute getTestClass_IntAttribute2();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CompanyFactory getCompanyFactory();

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
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.DepartmentImpl <em>Department</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.DepartmentImpl
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getDepartment()
		 * @generated
		 */
		EClass DEPARTMENT = eINSTANCE.getDepartment();

		/**
		 * The meta object literal for the '<em><b>Number</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DEPARTMENT__NUMBER = eINSTANCE.getDepartment_Number();

		/**
		 * The meta object literal for the '<em><b>Employees</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DEPARTMENT__EMPLOYEES = eINSTANCE.getDepartment_Employees();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.EmployeeImpl <em>Employee</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.EmployeeImpl
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getEmployee()
		 * @generated
		 */
		EClass EMPLOYEE = eINSTANCE.getEmployee();

		/**
		 * The meta object literal for the '<em><b>First Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EMPLOYEE__FIRST_NAME = eINSTANCE.getEmployee_FirstName();

		/**
		 * The meta object literal for the '<em><b>Last Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EMPLOYEE__LAST_NAME = eINSTANCE.getEmployee_LastName();

		/**
		 * The meta object literal for the '<em><b>Age</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute EMPLOYEE__AGE = eINSTANCE.getEmployee_Age();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyImpl <em>Company</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyImpl
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getCompany()
		 * @generated
		 */
		EClass COMPANY = eINSTANCE.getCompany();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COMPANY__NAME = eINSTANCE.getCompany_Name();

		/**
		 * The meta object literal for the '<em><b>Departments</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference COMPANY__DEPARTMENTS = eINSTANCE.getCompany_Departments();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl <em>Test Class</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.TestClassImpl
		 * @see org.eclipse.wazaabi.engine.edp.tests.model.company.impl.CompanyPackageImpl#getTestClass()
		 * @generated
		 */
		EClass TEST_CLASS = eINSTANCE.getTestClass();

		/**
		 * The meta object literal for the '<em><b>String Attribute1</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_CLASS__STRING_ATTRIBUTE1 = eINSTANCE.getTestClass_StringAttribute1();

		/**
		 * The meta object literal for the '<em><b>String Attribute2</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_CLASS__STRING_ATTRIBUTE2 = eINSTANCE.getTestClass_StringAttribute2();

		/**
		 * The meta object literal for the '<em><b>Int Attribute1</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_CLASS__INT_ATTRIBUTE1 = eINSTANCE.getTestClass_IntAttribute1();

		/**
		 * The meta object literal for the '<em><b>Int Attribute2</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TEST_CLASS__INT_ATTRIBUTE2 = eINSTANCE.getTestClass_IntAttribute2();

	}

} //CompanyPackage
