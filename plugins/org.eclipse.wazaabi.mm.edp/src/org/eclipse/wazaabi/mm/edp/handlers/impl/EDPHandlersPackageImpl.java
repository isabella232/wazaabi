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
package org.eclipse.wazaabi.mm.edp.handlers.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.edp.EdpPackage;

import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;

import org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsPackageImpl;

import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.BooleanParameter;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.Converter;
import org.eclipse.wazaabi.mm.edp.handlers.Deferred;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;
import org.eclipse.wazaabi.mm.edp.handlers.IntParameter;
import org.eclipse.wazaabi.mm.edp.handlers.Operation;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;
import org.eclipse.wazaabi.mm.edp.handlers.Sequence;
import org.eclipse.wazaabi.mm.edp.handlers.State;
import org.eclipse.wazaabi.mm.edp.handlers.StringParameter;
import org.eclipse.wazaabi.mm.edp.handlers.Validator;

import org.eclipse.wazaabi.mm.edp.impl.EdpPackageImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class EDPHandlersPackageImpl extends EPackageImpl implements EDPHandlersPackage
{
  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass actionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass bindingEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass operationEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass executableEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass sequenceEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass deferredEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass eventHandlerEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass parameterizedEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass parameterEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass stringParameterEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass booleanParameterEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass intParameterEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass conditionEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass validatorEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EClass converterEClass = null;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  private EEnum stateEEnum = null;

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
   * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#eNS_URI
   * @see #init()
   * @generated
   */
  private EDPHandlersPackageImpl()
  {
    super(eNS_URI, EDPHandlersFactory.eINSTANCE);
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
   * <p>This method is used to initialize {@link EDPHandlersPackage#eINSTANCE} when that field is accessed.
   * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #eNS_URI
   * @see #createPackageContents()
   * @see #initializePackageContents()
   * @generated
   */
  public static EDPHandlersPackage init()
  {
    if (isInited) return (EDPHandlersPackage)EPackage.Registry.INSTANCE.getEPackage(EDPHandlersPackage.eNS_URI);

    // Obtain or create and register package
    EDPHandlersPackageImpl theEDPHandlersPackage = (EDPHandlersPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof EDPHandlersPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new EDPHandlersPackageImpl());

    isInited = true;

    // Obtain or create and register interdependencies
    EdpPackageImpl theEdpPackage = (EdpPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(EdpPackage.eNS_URI) instanceof EdpPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(EdpPackage.eNS_URI) : EdpPackage.eINSTANCE);
    EDPEventsPackageImpl theEDPEventsPackage = (EDPEventsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(EDPEventsPackage.eNS_URI) instanceof EDPEventsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(EDPEventsPackage.eNS_URI) : EDPEventsPackage.eINSTANCE);

    // Create package meta-data objects
    theEDPHandlersPackage.createPackageContents();
    theEdpPackage.createPackageContents();
    theEDPEventsPackage.createPackageContents();

    // Initialize created meta-data
    theEDPHandlersPackage.initializePackageContents();
    theEdpPackage.initializePackageContents();
    theEDPEventsPackage.initializePackageContents();

    // Mark meta-data to indicate it can't be changed
    theEDPHandlersPackage.freeze();

  
    // Update the registry and return the package
    EPackage.Registry.INSTANCE.put(EDPHandlersPackage.eNS_URI, theEDPHandlersPackage);
    return theEDPHandlersPackage;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getAction()
  {
    return actionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getBinding()
  {
    return bindingEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getOperation()
  {
    return operationEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getOperation_Async()
  {
    return (EAttribute)operationEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getExecutable()
  {
    return executableEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getExecutable_Id()
  {
    return (EAttribute)executableEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getSequence()
  {
    return sequenceEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getSequence_Executables()
  {
    return (EReference)sequenceEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getDeferred()
  {
    return deferredEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getDeferred_Uri()
  {
    return (EAttribute)deferredEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getEventHandler()
  {
    return eventHandlerEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEventHandler_Events()
  {
    return (EReference)eventHandlerEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getEventHandler_Conditions()
  {
    return (EReference)eventHandlerEClass.getEStructuralFeatures().get(1);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getParameterized()
  {
    return parameterizedEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EReference getParameterized_Parameters()
  {
    return (EReference)parameterizedEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getParameter()
  {
    return parameterEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getParameter_Name()
  {
    return (EAttribute)parameterEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getStringParameter()
  {
    return stringParameterEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getStringParameter_Value()
  {
    return (EAttribute)stringParameterEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getBooleanParameter()
  {
    return booleanParameterEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getBooleanParameter_Value()
  {
    return (EAttribute)booleanParameterEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getIntParameter()
  {
    return intParameterEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EAttribute getIntParameter_Value()
  {
    return (EAttribute)intParameterEClass.getEStructuralFeatures().get(0);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getCondition()
  {
    return conditionEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getValidator()
  {
    return validatorEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EClass getConverter()
  {
    return converterEClass;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EEnum getState()
  {
    return stateEEnum;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPHandlersFactory getEDPHandlersFactory()
  {
    return (EDPHandlersFactory)getEFactoryInstance();
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
    actionEClass = createEClass(ACTION);

    bindingEClass = createEClass(BINDING);

    operationEClass = createEClass(OPERATION);
    createEAttribute(operationEClass, OPERATION__ASYNC);

    executableEClass = createEClass(EXECUTABLE);
    createEAttribute(executableEClass, EXECUTABLE__ID);

    sequenceEClass = createEClass(SEQUENCE);
    createEReference(sequenceEClass, SEQUENCE__EXECUTABLES);

    deferredEClass = createEClass(DEFERRED);
    createEAttribute(deferredEClass, DEFERRED__URI);

    eventHandlerEClass = createEClass(EVENT_HANDLER);
    createEReference(eventHandlerEClass, EVENT_HANDLER__EVENTS);
    createEReference(eventHandlerEClass, EVENT_HANDLER__CONDITIONS);

    parameterizedEClass = createEClass(PARAMETERIZED);
    createEReference(parameterizedEClass, PARAMETERIZED__PARAMETERS);

    parameterEClass = createEClass(PARAMETER);
    createEAttribute(parameterEClass, PARAMETER__NAME);

    stringParameterEClass = createEClass(STRING_PARAMETER);
    createEAttribute(stringParameterEClass, STRING_PARAMETER__VALUE);

    booleanParameterEClass = createEClass(BOOLEAN_PARAMETER);
    createEAttribute(booleanParameterEClass, BOOLEAN_PARAMETER__VALUE);

    intParameterEClass = createEClass(INT_PARAMETER);
    createEAttribute(intParameterEClass, INT_PARAMETER__VALUE);

    conditionEClass = createEClass(CONDITION);

    validatorEClass = createEClass(VALIDATOR);

    converterEClass = createEClass(CONVERTER);

    // Create enums
    stateEEnum = createEEnum(STATE);
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

    // Create type parameters

    // Set bounds for type parameters

    // Add supertypes to classes
    actionEClass.getESuperTypes().add(this.getOperation());
    bindingEClass.getESuperTypes().add(this.getEventHandler());
    operationEClass.getESuperTypes().add(this.getDeferred());
    operationEClass.getESuperTypes().add(this.getExecutable());
    sequenceEClass.getESuperTypes().add(this.getExecutable());
    eventHandlerEClass.getESuperTypes().add(this.getParameterized());
    eventHandlerEClass.getESuperTypes().add(this.getSequence());
    eventHandlerEClass.getESuperTypes().add(this.getAction());
    stringParameterEClass.getESuperTypes().add(this.getParameter());
    booleanParameterEClass.getESuperTypes().add(this.getParameter());
    intParameterEClass.getESuperTypes().add(this.getParameter());
    conditionEClass.getESuperTypes().add(this.getValidator());
    validatorEClass.getESuperTypes().add(this.getOperation());
    converterEClass.getESuperTypes().add(this.getAction());

    // Initialize classes and features; add operations and parameters
    initEClass(actionEClass, Action.class, "Action", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(bindingEClass, Binding.class, "Binding", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(operationEClass, Operation.class, "Operation", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getOperation_Async(), ecorePackage.getEBoolean(), "async", "false", 0, 1, Operation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(executableEClass, Executable.class, "Executable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getExecutable_Id(), ecorePackage.getEString(), "id", null, 0, 1, Executable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(sequenceEClass, Sequence.class, "Sequence", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getSequence_Executables(), this.getExecutable(), null, "executables", null, 0, -1, Sequence.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(deferredEClass, Deferred.class, "Deferred", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getDeferred_Uri(), ecorePackage.getEString(), "uri", null, 0, 1, Deferred.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(eventHandlerEClass, EventHandler.class, "EventHandler", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getEventHandler_Events(), theEDPEventsPackage.getEvent(), null, "events", null, 0, -1, EventHandler.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    initEReference(getEventHandler_Conditions(), this.getCondition(), null, "conditions", null, 0, -1, EventHandler.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(parameterizedEClass, Parameterized.class, "Parameterized", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEReference(getParameterized_Parameters(), this.getParameter(), null, "parameters", null, 0, -1, Parameterized.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(parameterEClass, Parameter.class, "Parameter", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getParameter_Name(), ecorePackage.getEString(), "name", null, 0, 1, Parameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(stringParameterEClass, StringParameter.class, "StringParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getStringParameter_Value(), ecorePackage.getEString(), "value", null, 0, 1, StringParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(booleanParameterEClass, BooleanParameter.class, "BooleanParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getBooleanParameter_Value(), ecorePackage.getEBoolean(), "value", null, 0, 1, BooleanParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(intParameterEClass, IntParameter.class, "IntParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
    initEAttribute(getIntParameter_Value(), ecorePackage.getEInt(), "value", null, 0, 1, IntParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

    initEClass(conditionEClass, Condition.class, "Condition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(validatorEClass, Validator.class, "Validator", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    initEClass(converterEClass, Converter.class, "Converter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

    // Initialize enums and add enum literals
    initEEnum(stateEEnum, State.class, "State");
  }

} //EDPHandlersPackageImpl
