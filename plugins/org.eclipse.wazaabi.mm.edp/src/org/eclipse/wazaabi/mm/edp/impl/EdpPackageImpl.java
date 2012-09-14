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
package org.eclipse.wazaabi.mm.edp.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.edp.Context;
import org.eclipse.wazaabi.mm.edp.ContextContent;
import org.eclipse.wazaabi.mm.edp.EdpFactory;
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;

import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;

import org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl;

import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;

import org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class EdpPackageImpl extends EPackageImpl implements EdpPackage
{
  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass contextEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass contextContentEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass eventDispatcherEClass = null;

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
   * @see org.eclipse.wazaabi.mm.edp.EdpPackage#eNS_URI
   * @see #init()
   * @generated
   */
  private EdpPackageImpl()
  {
    super(eNS_URI, EdpFactory.eINSTANCE);
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
   * <p>This method is used to initialize {@link EdpPackage#eINSTANCE} when that field is accessed.
   * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #eNS_URI
   * @see #createPackageContents()
   * @see #initializePackageContents()
   * @generated
   */
  public static EdpPackage init()
  {
    if (isInited) return (EdpPackage)EPackage.Registry.INSTANCE.getEPackage(EdpPackage.eNS_URI);

    // Obtain or create and register package
    EdpPackageImpl theEdpPackage = (EdpPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof EdpPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new EdpPackageImpl());

    isInited = true;

    // Obtain or create and register interdependencies
    EDPEventsPackageImpl theEDPEventsPackage = (EDPEventsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(EDPEventsPackage.eNS_URI) instanceof EDPEventsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(EDPEventsPackage.eNS_URI) : EDPEventsPackage.eINSTANCE);
    EDPHandlersPackageImpl theEDPHandlersPackage = (EDPHandlersPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI) instanceof EDPHandlersPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI) : EDPHandlersPackage.eINSTANCE);

    // Create package meta-data objects
    theEdpPackage.createPackageContents();
    theEDPEventsPackage.createPackageContents();
    theEDPHandlersPackage.createPackageContents();

    // Initialize created meta-data
    theEdpPackage.initializePackageContents();
    theEDPEventsPackage.initializePackageContents();
    theEDPHandlersPackage.initializePackageContents();

    // Mark meta-data to indicate it can't be changed
    theEdpPackage.freeze();

  
    // Update the registry and return the package
    EPackage.Registry.INSTANCE.put(EdpPackage.eNS_URI, theEdpPackage);
    return theEdpPackage;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getContext()
  {
    return contextEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getContext_Contents()
  {
    return (EReference)contextEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getContextContent()
  {
    return contextContentEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getContextContent_Key()
  {
    return (EAttribute)contextContentEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getContextContent_Value()
  {
    return (EAttribute)contextContentEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEventDispatcher()
  {
    return eventDispatcherEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEventDispatcher_Handlers()
  {
    return (EReference)eventDispatcherEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getEventDispatcher_State()
  {
    return (EAttribute)eventDispatcherEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EdpFactory getEdpFactory()
  {
    return (EdpFactory)getEFactoryInstance();
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
    contextEClass = createEClass(CONTEXT);
    createEReference(contextEClass, CONTEXT__CONTENTS);

    contextContentEClass = createEClass(CONTEXT_CONTENT);
    createEAttribute(contextContentEClass, CONTEXT_CONTENT__KEY);
    createEAttribute(contextContentEClass, CONTEXT_CONTENT__VALUE);

    eventDispatcherEClass = createEClass(EVENT_DISPATCHER);
    createEReference(eventDispatcherEClass, EVENT_DISPATCHER__HANDLERS);
    createEAttribute(eventDispatcherEClass, EVENT_DISPATCHER__STATE);
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
    EDPEventsPackage theEDPEventsPackage = (EDPEventsPackage)EPackage.Registry.INSTANCE.getEPackage(EDPEventsPackage.eNS_URI);
    EDPHandlersPackage theEDPHandlersPackage = (EDPHandlersPackage)EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI);

    // Add subpackages
    getESubpackages().add(theEDPEventsPackage);
    getESubpackages().add(theEDPHandlersPackage);

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    eventDispatcherEClass.getESuperTypes().add(this.getContext());

    // Initialize classes and features; add operations and parameters
    initEClass(contextEClass, Context.class, "Context", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getContext_Contents(), this.getContextContent(), null, "contents", null, 0, -1, Context.class, IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    EOperation op = addEOperation(contextEClass, ecorePackage.getEBoolean(), "containsKey", 0, 1, IS_UNIQUE, IS_ORDERED);
    addEParameter(op, ecorePackage.getEString(), "key", 1, 1, IS_UNIQUE, IS_ORDERED);

    op = addEOperation(contextEClass, ecorePackage.getEBoolean(), "containsKey", 0, 1, IS_UNIQUE, IS_ORDERED);
    addEParameter(op, ecorePackage.getEString(), "key", 1, 1, IS_UNIQUE, IS_ORDERED);
    addEParameter(op, ecorePackage.getEBoolean(), "local", 0, 1, IS_UNIQUE, IS_ORDERED);

    op = addEOperation(contextEClass, ecorePackage.getEJavaObject(), "get", 0, 1, IS_UNIQUE, IS_ORDERED);
    addEParameter(op, ecorePackage.getEString(), "key", 1, 1, IS_UNIQUE, IS_ORDERED);

    op = addEOperation(contextEClass, null, "remove", 0, 1, IS_UNIQUE, IS_ORDERED);
    addEParameter(op, ecorePackage.getEString(), "key", 1, 1, IS_UNIQUE, IS_ORDERED);

    op = addEOperation(contextEClass, null, "set", 0, 1, IS_UNIQUE, IS_ORDERED);
    addEParameter(op, ecorePackage.getEString(), "key", 1, 1, IS_UNIQUE, IS_ORDERED);
    addEParameter(op, ecorePackage.getEJavaObject(), "value", 0, 1, IS_UNIQUE, IS_ORDERED);

    initEClass(contextContentEClass, ContextContent.class, "ContextContent", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getContextContent_Key(), ecorePackage.getEString(), "key", null, 1, 1, ContextContent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getContextContent_Value(), ecorePackage.getEJavaObject(), "value", null, 0, 1, ContextContent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(eventDispatcherEClass, EventDispatcher.class, "EventDispatcher", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getEventDispatcher_Handlers(), theEDPHandlersPackage.getEventHandler(), null, "handlers", null, 0, -1, EventDispatcher.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEAttribute(getEventDispatcher_State(), theEDPHandlersPackage.getState(), "state", null, 0, 1, EventDispatcher.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    // Create resource
    createResource(eNS_URI);
  }

} //EdpPackageImpl
