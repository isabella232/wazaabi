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
package org.eclipse.wazaabi.mm.core.themes.Themes;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

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
 * @see org.eclipse.wazaabi.mm.core.themes.Themes.CoreThemesFactory
 * @model kind="package"
 * @generated
 */
public interface CoreThemesPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "Themes";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/core/themes";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "thms";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CoreThemesPackage eINSTANCE = org.eclipse.wazaabi.mm.core.themes.Themes.impl.CoreThemesPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.themes.Themes.impl.BlankWidgetImpl <em>Blank Widget</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.themes.Themes.impl.BlankWidgetImpl
	 * @see org.eclipse.wazaabi.mm.core.themes.Themes.impl.CoreThemesPackageImpl#getBlankWidget()
	 * @generated
	 */
	int BLANK_WIDGET = 0;

	/**
	 * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_WIDGET__ANNOTATIONS = CoreWidgetsPackage.WIDGET__ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Contents</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_WIDGET__CONTENTS = CoreWidgetsPackage.WIDGET__CONTENTS;

	/**
	 * The feature id for the '<em><b>Handlers</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_WIDGET__HANDLERS = CoreWidgetsPackage.WIDGET__HANDLERS;

	/**
	 * The feature id for the '<em><b>State</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_WIDGET__STATE = CoreWidgetsPackage.WIDGET__STATE;

	/**
	 * The feature id for the '<em><b>Style Rules</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_WIDGET__STYLE_RULES = CoreWidgetsPackage.WIDGET__STYLE_RULES;

	/**
	 * The number of structural features of the '<em>Blank Widget</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BLANK_WIDGET_FEATURE_COUNT = CoreWidgetsPackage.WIDGET_FEATURE_COUNT + 0;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.themes.Themes.BlankWidget <em>Blank Widget</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Blank Widget</em>'.
	 * @see org.eclipse.wazaabi.mm.core.themes.Themes.BlankWidget
	 * @generated
	 */
	EClass getBlankWidget();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CoreThemesFactory getCoreThemesFactory();

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
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.themes.Themes.impl.BlankWidgetImpl <em>Blank Widget</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.themes.Themes.impl.BlankWidgetImpl
		 * @see org.eclipse.wazaabi.mm.core.themes.Themes.impl.CoreThemesPackageImpl#getBlankWidget()
		 * @generated
		 */
		EClass BLANK_WIDGET = eINSTANCE.getBlankWidget();

	}

} //CoreThemesPackage
