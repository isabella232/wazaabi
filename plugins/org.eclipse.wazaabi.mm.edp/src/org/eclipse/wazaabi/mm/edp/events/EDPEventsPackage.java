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
package org.eclipse.wazaabi.mm.edp.events;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.wazaabi.mm.edp.EdpPackage;

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
 * @see org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory
 * @model kind="package"
 * @generated
 */
public interface EDPEventsPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "events";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.wazaabi.org/edp/events";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "edpevts";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  EDPEventsPackage eINSTANCE = org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl.init();

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.EventImpl <em>Event</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.events.impl.EventImpl
   * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getEvent()
   * @generated
   */
  int EVENT = 2;

  /**
   * The feature id for the '<em><b>Contents</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__CONTENTS = EdpPackage.CONTEXT__CONTENTS;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT__ID = EdpPackage.CONTEXT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Event</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_FEATURE_COUNT = EdpPackage.CONTEXT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.PathEventImpl <em>Path Event</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.events.impl.PathEventImpl
   * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getPathEvent()
   * @generated
   */
  int PATH_EVENT = 0;

  /**
   * The feature id for the '<em><b>Contents</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PATH_EVENT__CONTENTS = EVENT__CONTENTS;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PATH_EVENT__ID = EVENT__ID;

  /**
   * The feature id for the '<em><b>Path</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PATH_EVENT__PATH = EVENT_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Path Event</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PATH_EVENT_FEATURE_COUNT = EVENT_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.ContentChangedEventImpl <em>Content Changed Event</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.events.impl.ContentChangedEventImpl
   * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getContentChangedEvent()
   * @generated
   */
  int CONTENT_CHANGED_EVENT = 1;

  /**
   * The feature id for the '<em><b>Contents</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTENT_CHANGED_EVENT__CONTENTS = PATH_EVENT__CONTENTS;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTENT_CHANGED_EVENT__ID = PATH_EVENT__ID;

  /**
   * The feature id for the '<em><b>Path</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTENT_CHANGED_EVENT__PATH = PATH_EVENT__PATH;

  /**
   * The number of structural features of the '<em>Content Changed Event</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONTENT_CHANGED_EVENT_FEATURE_COUNT = PATH_EVENT_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.PropertyChangedEventImpl <em>Property Changed Event</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.events.impl.PropertyChangedEventImpl
   * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getPropertyChangedEvent()
   * @generated
   */
  int PROPERTY_CHANGED_EVENT = 3;

  /**
   * The feature id for the '<em><b>Contents</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_CHANGED_EVENT__CONTENTS = PATH_EVENT__CONTENTS;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_CHANGED_EVENT__ID = PATH_EVENT__ID;

  /**
   * The feature id for the '<em><b>Path</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_CHANGED_EVENT__PATH = PATH_EVENT__PATH;

  /**
   * The number of structural features of the '<em>Property Changed Event</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PROPERTY_CHANGED_EVENT_FEATURE_COUNT = PATH_EVENT_FEATURE_COUNT + 0;


  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.events.PathEvent <em>Path Event</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Path Event</em>'.
   * @see org.eclipse.wazaabi.mm.edp.events.PathEvent
   * @generated
   */
  EClass getPathEvent();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.events.PathEvent#getPath <em>Path</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Path</em>'.
   * @see org.eclipse.wazaabi.mm.edp.events.PathEvent#getPath()
   * @see #getPathEvent()
   * @generated
   */
  EAttribute getPathEvent_Path();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.events.ContentChangedEvent <em>Content Changed Event</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Content Changed Event</em>'.
   * @see org.eclipse.wazaabi.mm.edp.events.ContentChangedEvent
   * @generated
   */
  EClass getContentChangedEvent();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.events.Event <em>Event</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Event</em>'.
   * @see org.eclipse.wazaabi.mm.edp.events.Event
   * @generated
   */
  EClass getEvent();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.events.Event#getId <em>Id</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Id</em>'.
   * @see org.eclipse.wazaabi.mm.edp.events.Event#getId()
   * @see #getEvent()
   * @generated
   */
  EAttribute getEvent_Id();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent <em>Property Changed Event</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Property Changed Event</em>'.
   * @see org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent
   * @generated
   */
  EClass getPropertyChangedEvent();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  EDPEventsFactory getEDPEventsFactory();

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
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.PathEventImpl <em>Path Event</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.events.impl.PathEventImpl
     * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getPathEvent()
     * @generated
     */
    EClass PATH_EVENT = eINSTANCE.getPathEvent();

    /**
     * The meta object literal for the '<em><b>Path</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute PATH_EVENT__PATH = eINSTANCE.getPathEvent_Path();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.ContentChangedEventImpl <em>Content Changed Event</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.events.impl.ContentChangedEventImpl
     * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getContentChangedEvent()
     * @generated
     */
    EClass CONTENT_CHANGED_EVENT = eINSTANCE.getContentChangedEvent();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.EventImpl <em>Event</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.events.impl.EventImpl
     * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getEvent()
     * @generated
     */
    EClass EVENT = eINSTANCE.getEvent();

    /**
     * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute EVENT__ID = eINSTANCE.getEvent_Id();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.events.impl.PropertyChangedEventImpl <em>Property Changed Event</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.events.impl.PropertyChangedEventImpl
     * @see org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl#getPropertyChangedEvent()
     * @generated
     */
    EClass PROPERTY_CHANGED_EVENT = eINSTANCE.getPropertyChangedEvent();

  }

} //EDPEventsPackage
