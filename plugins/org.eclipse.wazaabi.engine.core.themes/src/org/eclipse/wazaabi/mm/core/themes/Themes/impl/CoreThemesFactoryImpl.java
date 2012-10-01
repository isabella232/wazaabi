/**
 *  Copyright (c) 2012 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.wazaabi.mm.core.themes.Themes.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.core.themes.Themes.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreThemesFactoryImpl extends EFactoryImpl implements CoreThemesFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static CoreThemesFactory init() {
		try {
			CoreThemesFactory theCoreThemesFactory = (CoreThemesFactory)EPackage.Registry.INSTANCE.getEFactory("http://www.wazaabi.org/core/themes"); 
			if (theCoreThemesFactory != null) {
				return theCoreThemesFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new CoreThemesFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreThemesFactoryImpl() {
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
			case CoreThemesPackage.BLANK_WIDGET: return createBlankWidget();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BlankWidget createBlankWidget() {
		BlankWidgetImpl blankWidget = new BlankWidgetImpl();
		return blankWidget;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreThemesPackage getCoreThemesPackage() {
		return (CoreThemesPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static CoreThemesPackage getPackage() {
		return CoreThemesPackage.eINSTANCE;
	}

} //CoreThemesFactoryImpl
