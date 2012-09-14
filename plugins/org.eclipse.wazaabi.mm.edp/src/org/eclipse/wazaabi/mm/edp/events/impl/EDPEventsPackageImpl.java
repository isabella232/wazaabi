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
package org.eclipse.wazaabi.mm.edp.events.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.edp.EdpPackage;

import org.eclipse.wazaabi.mm.edp.events.ContentChangedEvent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsFactory;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.events.PathEvent;
import org.eclipse.wazaabi.mm.edp.events.PropertyChangedEvent;

import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

import org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl;

import org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class EDPEventsPackageImpl extends EPackageImpl implements EDPEventsPackage
{
  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass pathEventEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass contentChangedEventEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass eventEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass propertyChangedEventEClass = null;

  /**
   * Creates an instance of the model <b>Package</b>, registered with
   * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
   * package URI value.
   * <p>Note: the correct way to create the package is via the static
   * factory method {@link #init init()}, which also performs
   * initialization of the package, or returns the registered package,
   * if one already exists.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.emf.ecore.EPackage.Registry
   * @see org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage#eNS_URI
   * @see #init()
   * @generated
   */
  private EDPEventsPackageImpl()
  {
    super(eNS_URI, EDPEventsFactory.eINSTANCE);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private static boolean isInited = false;

  /**
   * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
   * 
   * <p>This method is used to initialize {@link EDPEventsPackage#eINSTANCE} when that field is accessed.
   * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #eNS_URI
   * @see #createPackageContents()
   * @see #initializePackageContents()
   * @generated
   */
  public static EDPEventsPackage init()
  {
    if (isInited) return (EDPEventsPackage)EPackage.Registry.INSTANCE.getEPackage(EDPEventsPackage.eNS_URI);

    // Obtain or create and register package
    EDPEventsPackageImpl theEDPEventsPackage = (EDPEventsPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof EDPEventsPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new EDPEventsPackageImpl());

    isInited = true;

    // Obtain or create and register interdependencies
    EdpPackageImpl theEdpPackage = (EdpPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(EdpPackage.eNS_URI) instanceof EdpPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(EdpPackage.eNS_URI) : EdpPackage.eINSTANCE);
    EDPHandlersPackageImpl theEDPHandlersPackage = (EDPHandlersPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI) instanceof EDPHandlersPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI) : EDPHandlersPackage.eINSTANCE);

    // Create package meta-data objects
    theEDPEventsPackage.createPackageContents();
    theEdpPackage.createPackageContents();
    theEDPHandlersPackage.createPackageContents();

    // Initialize created meta-data
    theEDPEventsPackage.initializePackageContents();
    theEdpPackage.initializePackageContents();
    theEDPHandlersPackage.initializePackageContents();

    // Mark meta-data to indicate it can't be changed
    theEDPEventsPackage.freeze();

  
    // Update the registry and return the package
    EPackage.Registry.INSTANCE.put(EDPEventsPackage.eNS_URI, theEDPEventsPackage);
    return theEDPEventsPackage;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getPathEvent()
  {
    return pathEventEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getPathEvent_Path()
  {
    return (EAttribute)pathEventEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getContentChangedEvent()
  {
    return contentChangedEventEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEvent()
  {
    return eventEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getEvent_Id()
  {
    return (EAttribute)eventEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getPropertyChangedEvent()
  {
    return propertyChangedEventEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPEventsFactory getEDPEventsFactory()
  {
    return (EDPEventsFactory)getEFactoryInstance();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private boolean isCreated = false;

  /**
   * Creates the meta-model objects for the package.  This method is
   * guarded to have no affect on any invocation but its first.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void createPackageContents()
  {
    if (isCreated) return;
    isCreated = true;

    // Create classes and their features
    pathEventEClass = createEClass(PATH_EVENT);
    createEAttribute(pathEventEClass, PATH_EVENT__PATH);

    contentChangedEventEClass = createEClass(CONTENT_CHANGED_EVENT);

    eventEClass = createEClass(EVENT);
    createEAttribute(eventEClass, EVENT__ID);

    propertyChangedEventEClass = createEClass(PROPERTY_CHANGED_EVENT);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private boolean isInitialized = false;

  /**
   * Complete the initialization of the package and its meta-model.  This
   * method is guarded to have no affect on any invocation but its first.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void initializePackageContents()
  {
    if (isInitialized) return;
    isInitialized = true;

    // Initialize package
    setName(eNAME);
    setNsPrefix(eNS_PREFIX);
    setNsURI(eNS_URI);

    // Obtain other dependent packages
    EdpPackage theEdpPackage = (EdpPackage)EPackage.Registry.INSTANCE.getEPackage(EdpPackage.eNS_URI);

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    pathEventEClass.getESuperTypes().add(this.getEvent());
    contentChangedEventEClass.getESuperTypes().add(this.getPathEvent());
    eventEClass.getESuperTypes().add(theEdpPackage.getContext());
    propertyChangedEventEClass.getESuperTypes().add(this.getPathEvent());

    // Initialize classes and features; add operations and parameters
    initEClass(pathEventEClass, PathEvent.class, "PathEvent", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getPathEvent_Path(), ecorePackage.getEString(), "path", null, 0, 1, PathEvent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(contentChangedEventEClass, ContentChangedEvent.class, "ContentChangedEvent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(eventEClass, Event.class, "Event", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getEvent_Id(), ecorePackage.getEString(), "id", null, 0, 1, Event.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(propertyChangedEventEClass, PropertyChangedEvent.class, "PropertyChangedEvent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
  }

} //EDPEventsPackageImpl
