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
package org.eclipse.wazaabi.mm.core;

import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;

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
 * @see org.eclipse.wazaabi.mm.core.CoreFactory
 * @model kind="package"
 * @generated
 */
public interface CorePackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "core";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/core";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "wc";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CorePackage eINSTANCE = org.eclipse.wazaabi.mm.core.impl.CorePackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.HorizontalAlignment <em>Horizontal Alignment</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.HorizontalAlignment
	 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getHorizontalAlignment()
	 * @generated
	 */
	int HORIZONTAL_ALIGNMENT = 0;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.Orientation <em>Orientation</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getOrientation()
	 * @generated
	 */
	int ORIENTATION = 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.Direction <em>Direction</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.Direction
	 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getDirection()
	 * @generated
	 */
	int DIRECTION = 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.Position <em>Position</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.Position
	 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getPosition()
	 * @generated
	 */
	int POSITION = 3;


	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.core.HorizontalAlignment <em>Horizontal Alignment</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Horizontal Alignment</em>'.
	 * @see org.eclipse.wazaabi.mm.core.HorizontalAlignment
	 * @generated
	 */
	EEnum getHorizontalAlignment();

	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.core.Orientation <em>Orientation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Orientation</em>'.
	 * @see org.eclipse.wazaabi.mm.core.Orientation
	 * @generated
	 */
	EEnum getOrientation();

	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.core.Direction <em>Direction</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Direction</em>'.
	 * @see org.eclipse.wazaabi.mm.core.Direction
	 * @generated
	 */
	EEnum getDirection();

	/**
	 * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.core.Position <em>Position</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Position</em>'.
	 * @see org.eclipse.wazaabi.mm.core.Position
	 * @generated
	 */
	EEnum getPosition();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CoreFactory getCoreFactory();

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
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.HorizontalAlignment <em>Horizontal Alignment</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.HorizontalAlignment
		 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getHorizontalAlignment()
		 * @generated
		 */
		EEnum HORIZONTAL_ALIGNMENT = eINSTANCE.getHorizontalAlignment();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.Orientation <em>Orientation</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.Orientation
		 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getOrientation()
		 * @generated
		 */
		EEnum ORIENTATION = eINSTANCE.getOrientation();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.Direction <em>Direction</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.Direction
		 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getDirection()
		 * @generated
		 */
		EEnum DIRECTION = eINSTANCE.getDirection();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.Position <em>Position</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.Position
		 * @see org.eclipse.wazaabi.mm.core.impl.CorePackageImpl#getPosition()
		 * @generated
		 */
		EEnum POSITION = eINSTANCE.getPosition();

	}

} //CorePackage
