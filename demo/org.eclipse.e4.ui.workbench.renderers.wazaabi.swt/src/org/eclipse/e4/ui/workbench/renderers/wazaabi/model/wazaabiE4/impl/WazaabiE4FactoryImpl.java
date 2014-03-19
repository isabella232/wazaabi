/**
 *  Copyright (c) 2014 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.impl;

import org.eclipse.e4.ui.workbench.renderers.wazaabi.model.wazaabiE4.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class WazaabiE4FactoryImpl extends EFactoryImpl implements WazaabiE4Factory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static WazaabiE4Factory init() {
		try {
			WazaabiE4Factory theWazaabiE4Factory = (WazaabiE4Factory)EPackage.Registry.INSTANCE.getEFactory(WazaabiE4Package.eNS_URI);
			if (theWazaabiE4Factory != null) {
				return theWazaabiE4Factory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new WazaabiE4FactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public WazaabiE4FactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case WazaabiE4Package.WAZAABI_PART: return createWazaabiPart();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public WazaabiPart createWazaabiPart() {
		WazaabiPartImpl wazaabiPart = new WazaabiPartImpl();
		return wazaabiPart;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public WazaabiE4Package getWazaabiE4Package() {
		return (WazaabiE4Package)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static WazaabiE4Package getPackage() {
		return WazaabiE4Package.eINSTANCE;
	}

} //WazaabiE4FactoryImpl
