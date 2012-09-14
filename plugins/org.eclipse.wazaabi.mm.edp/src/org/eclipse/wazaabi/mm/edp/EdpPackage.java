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
package org.eclipse.wazaabi.mm.edp;

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
 * @see org.eclipse.wazaabi.mm.edp.EdpFactory
 * @model kind="package"
 * @generated
 */
public interface EdpPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "edp";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.wazaabi.org/edp";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "edp";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  EdpPackage eINSTANCE = org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl.init();

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.Context <em>Context</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.Context
   * @see org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl#getContext()
   * @generated
   */
  int CONTEXT = 0;

  /**
   * The feature id for the '<em><b>Contents</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTEXT__CONTENTS = 0;

  /**
   * The number of structural features of the '<em>Context</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTEXT_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.impl.ContextContentImpl <em>Context Content</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.impl.ContextContentImpl
   * @see org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl#getContextContent()
   * @generated
   */
  int CONTEXT_CONTENT = 1;

  /**
   * The feature id for the '<em><b>Key</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTEXT_CONTENT__KEY = 0;

  /**
   * The feature id for the '<em><b>Value</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTEXT_CONTENT__VALUE = 1;

  /**
   * The number of structural features of the '<em>Context Content</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTEXT_CONTENT_FEATURE_COUNT = 2;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.impl.EventDispatcherImpl <em>Event Dispatcher</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.impl.EventDispatcherImpl
   * @see org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl#getEventDispatcher()
   * @generated
   */
  int EVENT_DISPATCHER = 2;

  /**
   * The feature id for the '<em><b>Contents</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_DISPATCHER__CONTENTS = CONTEXT__CONTENTS;

  /**
   * The feature id for the '<em><b>Handlers</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_DISPATCHER__HANDLERS = CONTEXT_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>State</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_DISPATCHER__STATE = CONTEXT_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Event Dispatcher</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_DISPATCHER_FEATURE_COUNT = CONTEXT_FEATURE_COUNT + 2;


  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.Context <em>Context</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Context</em>'.
   * @see org.eclipse.wazaabi.mm.edp.Context
   * @generated
   */
  EClass getContext();

  /**
   * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.edp.Context#getContents <em>Contents</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Contents</em>'.
   * @see org.eclipse.wazaabi.mm.edp.Context#getContents()
   * @see #getContext()
   * @generated
   */
  EReference getContext_Contents();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.ContextContent <em>Context Content</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Context Content</em>'.
   * @see org.eclipse.wazaabi.mm.edp.ContextContent
   * @generated
   */
  EClass getContextContent();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.ContextContent#getKey <em>Key</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Key</em>'.
   * @see org.eclipse.wazaabi.mm.edp.ContextContent#getKey()
   * @see #getContextContent()
   * @generated
   */
  EAttribute getContextContent_Key();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.ContextContent#getValue <em>Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Value</em>'.
   * @see org.eclipse.wazaabi.mm.edp.ContextContent#getValue()
   * @see #getContextContent()
   * @generated
   */
  EAttribute getContextContent_Value();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.EventDispatcher <em>Event Dispatcher</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Event Dispatcher</em>'.
   * @see org.eclipse.wazaabi.mm.edp.EventDispatcher
   * @generated
   */
  EClass getEventDispatcher();

  /**
   * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.edp.EventDispatcher#getHandlers <em>Handlers</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Handlers</em>'.
   * @see org.eclipse.wazaabi.mm.edp.EventDispatcher#getHandlers()
   * @see #getEventDispatcher()
   * @generated
   */
  EReference getEventDispatcher_Handlers();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.EventDispatcher#getState <em>State</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>State</em>'.
   * @see org.eclipse.wazaabi.mm.edp.EventDispatcher#getState()
   * @see #getEventDispatcher()
   * @generated
   */
  EAttribute getEventDispatcher_State();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  EdpFactory getEdpFactory();

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
  interface Literals
  {
    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.Context <em>Context</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.Context
     * @see org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl#getContext()
     * @generated
     */
    EClass CONTEXT = eINSTANCE.getContext();

    /**
     * The meta object literal for the '<em><b>Contents</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference CONTEXT__CONTENTS = eINSTANCE.getContext_Contents();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.impl.ContextContentImpl <em>Context Content</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.impl.ContextContentImpl
     * @see org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl#getContextContent()
     * @generated
     */
    EClass CONTEXT_CONTENT = eINSTANCE.getContextContent();

    /**
     * The meta object literal for the '<em><b>Key</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute CONTEXT_CONTENT__KEY = eINSTANCE.getContextContent_Key();

    /**
     * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute CONTEXT_CONTENT__VALUE = eINSTANCE.getContextContent_Value();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.impl.EventDispatcherImpl <em>Event Dispatcher</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.impl.EventDispatcherImpl
     * @see org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl#getEventDispatcher()
     * @generated
     */
    EClass EVENT_DISPATCHER = eINSTANCE.getEventDispatcher();

    /**
     * The meta object literal for the '<em><b>Handlers</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EVENT_DISPATCHER__HANDLERS = eINSTANCE.getEventDispatcher_Handlers();

    /**
     * The meta object literal for the '<em><b>State</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute EVENT_DISPATCHER__STATE = eINSTANCE.getEventDispatcher_State();

  }

} //EdpPackage
