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

package org.eclipse.wazaabi.ui.model.parts;

import org.eclipse.emf.ecore.EAttribute;
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
 * @see org.eclipse.wazaabi.ui.model.parts.PartsFactory
 * @model kind="package"
 * @generated
 */
public interface PartsPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "parts";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/ui/parts";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "wparts";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	PartsPackage eINSTANCE = org.eclipse.wazaabi.ui.model.parts.impl.PartsPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.ui.model.parts.impl.PageImpl <em>Page</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.ui.model.parts.impl.PageImpl
	 * @see org.eclipse.wazaabi.ui.model.parts.impl.PartsPackageImpl#getPage()
	 * @generated
	 */
	int PAGE = 0;

	/**
	 * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__ANNOTATIONS = CoreWidgetsPackage.CONTAINER__ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Contents</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__CONTENTS = CoreWidgetsPackage.CONTAINER__CONTENTS;

	/**
	 * The feature id for the '<em><b>Handlers</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__HANDLERS = CoreWidgetsPackage.CONTAINER__HANDLERS;

	/**
	 * The feature id for the '<em><b>State</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__STATE = CoreWidgetsPackage.CONTAINER__STATE;

	/**
	 * The feature id for the '<em><b>Style Rules</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__STYLE_RULES = CoreWidgetsPackage.CONTAINER__STYLE_RULES;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__ID = CoreWidgetsPackage.CONTAINER__ID;

	/**
	 * The feature id for the '<em><b>Focus</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__FOCUS = CoreWidgetsPackage.CONTAINER__FOCUS;

	/**
	 * The feature id for the '<em><b>Children</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__CHILDREN = CoreWidgetsPackage.CONTAINER__CHILDREN;

	/**
	 * The feature id for the '<em><b>Ui Selector</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__UI_SELECTOR = CoreWidgetsPackage.CONTAINER_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Selection Processor</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE__SELECTION_PROCESSOR = CoreWidgetsPackage.CONTAINER_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Page</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PAGE_FEATURE_COUNT = CoreWidgetsPackage.CONTAINER_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.ui.model.parts.impl.ProxyImpl <em>Proxy</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.ui.model.parts.impl.ProxyImpl
	 * @see org.eclipse.wazaabi.ui.model.parts.impl.PartsPackageImpl#getProxy()
	 * @generated
	 */
	int PROXY = 1;

	/**
	 * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__ANNOTATIONS = CoreWidgetsPackage.ABSTRACT_COMPONENT__ANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Contents</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__CONTENTS = CoreWidgetsPackage.ABSTRACT_COMPONENT__CONTENTS;

	/**
	 * The feature id for the '<em><b>Handlers</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__HANDLERS = CoreWidgetsPackage.ABSTRACT_COMPONENT__HANDLERS;

	/**
	 * The feature id for the '<em><b>State</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__STATE = CoreWidgetsPackage.ABSTRACT_COMPONENT__STATE;

	/**
	 * The feature id for the '<em><b>Style Rules</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__STYLE_RULES = CoreWidgetsPackage.ABSTRACT_COMPONENT__STYLE_RULES;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__ID = CoreWidgetsPackage.ABSTRACT_COMPONENT__ID;

	/**
	 * The feature id for the '<em><b>Focus</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__FOCUS = CoreWidgetsPackage.ABSTRACT_COMPONENT__FOCUS;

	/**
	 * The feature id for the '<em><b>Uri</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY__URI = CoreWidgetsPackage.ABSTRACT_COMPONENT_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Proxy</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROXY_FEATURE_COUNT = CoreWidgetsPackage.ABSTRACT_COMPONENT_FEATURE_COUNT + 1;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.ui.model.parts.Page <em>Page</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Page</em>'.
	 * @see org.eclipse.wazaabi.ui.model.parts.Page
	 * @generated
	 */
	EClass getPage();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.ui.model.parts.Page#getUiSelector <em>Ui Selector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ui Selector</em>'.
	 * @see org.eclipse.wazaabi.ui.model.parts.Page#getUiSelector()
	 * @see #getPage()
	 * @generated
	 */
	EAttribute getPage_UiSelector();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.ui.model.parts.Page#getSelectionProcessor <em>Selection Processor</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Selection Processor</em>'.
	 * @see org.eclipse.wazaabi.ui.model.parts.Page#getSelectionProcessor()
	 * @see #getPage()
	 * @generated
	 */
	EAttribute getPage_SelectionProcessor();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.ui.model.parts.Proxy <em>Proxy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Proxy</em>'.
	 * @see org.eclipse.wazaabi.ui.model.parts.Proxy
	 * @generated
	 */
	EClass getProxy();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.ui.model.parts.Proxy#getUri <em>Uri</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uri</em>'.
	 * @see org.eclipse.wazaabi.ui.model.parts.Proxy#getUri()
	 * @see #getProxy()
	 * @generated
	 */
	EAttribute getProxy_Uri();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	PartsFactory getPartsFactory();

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
		 * The meta object literal for the '{@link org.eclipse.wazaabi.ui.model.parts.impl.PageImpl <em>Page</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.ui.model.parts.impl.PageImpl
		 * @see org.eclipse.wazaabi.ui.model.parts.impl.PartsPackageImpl#getPage()
		 * @generated
		 */
		EClass PAGE = eINSTANCE.getPage();

		/**
		 * The meta object literal for the '<em><b>Ui Selector</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PAGE__UI_SELECTOR = eINSTANCE.getPage_UiSelector();

		/**
		 * The meta object literal for the '<em><b>Selection Processor</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PAGE__SELECTION_PROCESSOR = eINSTANCE.getPage_SelectionProcessor();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.ui.model.parts.impl.ProxyImpl <em>Proxy</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.ui.model.parts.impl.ProxyImpl
		 * @see org.eclipse.wazaabi.ui.model.parts.impl.PartsPackageImpl#getProxy()
		 * @generated
		 */
		EClass PROXY = eINSTANCE.getProxy();

		/**
		 * The meta object literal for the '<em><b>Uri</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PROXY__URI = eINSTANCE.getProxy_Uri();

	}

} //PartsPackage
