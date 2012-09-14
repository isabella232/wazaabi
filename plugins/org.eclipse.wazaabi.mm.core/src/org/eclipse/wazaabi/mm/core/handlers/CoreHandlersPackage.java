/**
 *  Copyright (c) 2008 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.wazaabi.mm.core.handlers;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

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
 * @see org.eclipse.wazaabi.mm.core.handlers.CoreHandlersFactory
 * @model kind="package"
 * @generated
 */
public interface CoreHandlersPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "handlers";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/core/handlers";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "wchdlrs";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CoreHandlersPackage eINSTANCE = org.eclipse.wazaabi.mm.core.handlers.impl.CoreHandlersPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.handlers.impl.RefreshActionImpl <em>Refresh Action</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.handlers.impl.RefreshActionImpl
	 * @see org.eclipse.wazaabi.mm.core.handlers.impl.CoreHandlersPackageImpl#getRefreshAction()
	 * @generated
	 */
	int REFRESH_ACTION = 0;

	/**
	 * The feature id for the '<em><b>Uri</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFRESH_ACTION__URI = EDPHandlersPackage.OPERATION__URI;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFRESH_ACTION__ID = EDPHandlersPackage.OPERATION__ID;

	/**
	 * The feature id for the '<em><b>Async</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFRESH_ACTION__ASYNC = EDPHandlersPackage.OPERATION__ASYNC;

	/**
	 * The number of structural features of the '<em>Refresh Action</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFRESH_ACTION_FEATURE_COUNT = EDPHandlersPackage.OPERATION_FEATURE_COUNT + 0;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.handlers.RefreshAction <em>Refresh Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Refresh Action</em>'.
	 * @see org.eclipse.wazaabi.mm.core.handlers.RefreshAction
	 * @generated
	 */
	EClass getRefreshAction();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CoreHandlersFactory getCoreHandlersFactory();

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
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.handlers.impl.RefreshActionImpl <em>Refresh Action</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.handlers.impl.RefreshActionImpl
		 * @see org.eclipse.wazaabi.mm.core.handlers.impl.CoreHandlersPackageImpl#getRefreshAction()
		 * @generated
		 */
		EClass REFRESH_ACTION = eINSTANCE.getRefreshAction();

	}

} //CoreHandlersPackage
