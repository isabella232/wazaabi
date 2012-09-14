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
package org.eclipse.wazaabi.mm.edp.handlers;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
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
 * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersFactory
 * @model kind="package"
 * @generated
 */
public interface EDPHandlersPackage extends EPackage
{
  /**
   * The package name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNAME = "handlers";

  /**
   * The package namespace URI.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_URI = "http://www.wazaabi.org/edp/handlers";

  /**
   * The package namespace name.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  String eNS_PREFIX = "edphdlrs";

  /**
   * The singleton instance of the package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  EDPHandlersPackage eINSTANCE = org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl.init();

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.DeferredImpl <em>Deferred</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.DeferredImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getDeferred()
   * @generated
   */
  int DEFERRED = 5;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DEFERRED__URI = 0;

  /**
   * The number of structural features of the '<em>Deferred</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int DEFERRED_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.OperationImpl <em>Operation</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.OperationImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getOperation()
   * @generated
   */
  int OPERATION = 2;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OPERATION__URI = DEFERRED__URI;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OPERATION__ID = DEFERRED_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Async</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OPERATION__ASYNC = DEFERRED_FEATURE_COUNT + 1;

  /**
   * The number of structural features of the '<em>Operation</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int OPERATION_FEATURE_COUNT = DEFERRED_FEATURE_COUNT + 2;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ActionImpl <em>Action</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ActionImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getAction()
   * @generated
   */
  int ACTION = 0;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION__URI = OPERATION__URI;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION__ID = OPERATION__ID;

  /**
   * The feature id for the '<em><b>Async</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION__ASYNC = OPERATION__ASYNC;

  /**
   * The number of structural features of the '<em>Action</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int ACTION_FEATURE_COUNT = OPERATION_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameterized <em>Parameterized</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameterized
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getParameterized()
   * @generated
   */
  int PARAMETERIZED = 7;

  /**
   * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETERIZED__PARAMETERS = 0;

  /**
   * The number of structural features of the '<em>Parameterized</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETERIZED_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl <em>Event Handler</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getEventHandler()
   * @generated
   */
  int EVENT_HANDLER = 6;

  /**
   * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER__PARAMETERS = PARAMETERIZED__PARAMETERS;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER__ID = PARAMETERIZED_FEATURE_COUNT + 0;

  /**
   * The feature id for the '<em><b>Executables</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER__EXECUTABLES = PARAMETERIZED_FEATURE_COUNT + 1;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER__URI = PARAMETERIZED_FEATURE_COUNT + 2;

  /**
   * The feature id for the '<em><b>Async</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER__ASYNC = PARAMETERIZED_FEATURE_COUNT + 3;

  /**
   * The feature id for the '<em><b>Events</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER__EVENTS = PARAMETERIZED_FEATURE_COUNT + 4;

  /**
   * The feature id for the '<em><b>Conditions</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER__CONDITIONS = PARAMETERIZED_FEATURE_COUNT + 5;

  /**
   * The number of structural features of the '<em>Event Handler</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EVENT_HANDLER_FEATURE_COUNT = PARAMETERIZED_FEATURE_COUNT + 6;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.BindingImpl <em>Binding</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.BindingImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getBinding()
   * @generated
   */
  int BINDING = 1;

  /**
   * The feature id for the '<em><b>Parameters</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING__PARAMETERS = EVENT_HANDLER__PARAMETERS;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING__ID = EVENT_HANDLER__ID;

  /**
   * The feature id for the '<em><b>Executables</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING__EXECUTABLES = EVENT_HANDLER__EXECUTABLES;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING__URI = EVENT_HANDLER__URI;

  /**
   * The feature id for the '<em><b>Async</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING__ASYNC = EVENT_HANDLER__ASYNC;

  /**
   * The feature id for the '<em><b>Events</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING__EVENTS = EVENT_HANDLER__EVENTS;

  /**
   * The feature id for the '<em><b>Conditions</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING__CONDITIONS = EVENT_HANDLER__CONDITIONS;

  /**
   * The number of structural features of the '<em>Binding</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BINDING_FEATURE_COUNT = EVENT_HANDLER_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ExecutableImpl <em>Executable</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ExecutableImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getExecutable()
   * @generated
   */
  int EXECUTABLE = 3;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXECUTABLE__ID = 0;

  /**
   * The number of structural features of the '<em>Executable</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int EXECUTABLE_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.SequenceImpl <em>Sequence</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.SequenceImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getSequence()
   * @generated
   */
  int SEQUENCE = 4;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SEQUENCE__ID = EXECUTABLE__ID;

  /**
   * The feature id for the '<em><b>Executables</b></em>' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SEQUENCE__EXECUTABLES = EXECUTABLE_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Sequence</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int SEQUENCE_FEATURE_COUNT = EXECUTABLE_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameter <em>Parameter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameter
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getParameter()
   * @generated
   */
  int PARAMETER = 8;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETER__NAME = 0;

  /**
   * The number of structural features of the '<em>Parameter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int PARAMETER_FEATURE_COUNT = 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.StringParameterImpl <em>String Parameter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.StringParameterImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getStringParameter()
   * @generated
   */
  int STRING_PARAMETER = 9;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int STRING_PARAMETER__NAME = PARAMETER__NAME;

  /**
   * The feature id for the '<em><b>Value</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int STRING_PARAMETER__VALUE = PARAMETER_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>String Parameter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int STRING_PARAMETER_FEATURE_COUNT = PARAMETER_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.BooleanParameterImpl <em>Boolean Parameter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.BooleanParameterImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getBooleanParameter()
   * @generated
   */
  int BOOLEAN_PARAMETER = 10;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BOOLEAN_PARAMETER__NAME = PARAMETER__NAME;

  /**
   * The feature id for the '<em><b>Value</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BOOLEAN_PARAMETER__VALUE = PARAMETER_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Boolean Parameter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int BOOLEAN_PARAMETER_FEATURE_COUNT = PARAMETER_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.IntParameterImpl <em>Int Parameter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.IntParameterImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getIntParameter()
   * @generated
   */
  int INT_PARAMETER = 11;

  /**
   * The feature id for the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INT_PARAMETER__NAME = PARAMETER__NAME;

  /**
   * The feature id for the '<em><b>Value</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INT_PARAMETER__VALUE = PARAMETER_FEATURE_COUNT + 0;

  /**
   * The number of structural features of the '<em>Int Parameter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int INT_PARAMETER_FEATURE_COUNT = PARAMETER_FEATURE_COUNT + 1;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ValidatorImpl <em>Validator</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ValidatorImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getValidator()
   * @generated
   */
  int VALIDATOR = 13;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALIDATOR__URI = OPERATION__URI;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALIDATOR__ID = OPERATION__ID;

  /**
   * The feature id for the '<em><b>Async</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALIDATOR__ASYNC = OPERATION__ASYNC;

  /**
   * The number of structural features of the '<em>Validator</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int VALIDATOR_FEATURE_COUNT = OPERATION_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ConditionImpl <em>Condition</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ConditionImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getCondition()
   * @generated
   */
  int CONDITION = 12;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONDITION__URI = VALIDATOR__URI;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONDITION__ID = VALIDATOR__ID;

  /**
   * The feature id for the '<em><b>Async</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONDITION__ASYNC = VALIDATOR__ASYNC;

  /**
   * The number of structural features of the '<em>Condition</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONDITION_FEATURE_COUNT = VALIDATOR_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ConverterImpl <em>Converter</em>}' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ConverterImpl
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getConverter()
   * @generated
   */
  int CONVERTER = 14;

  /**
   * The feature id for the '<em><b>Uri</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONVERTER__URI = ACTION__URI;

  /**
   * The feature id for the '<em><b>Id</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONVERTER__ID = ACTION__ID;

  /**
   * The feature id for the '<em><b>Async</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONVERTER__ASYNC = ACTION__ASYNC;

  /**
   * The number of structural features of the '<em>Converter</em>' class.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   * @ordered
   */
  int CONVERTER_FEATURE_COUNT = ACTION_FEATURE_COUNT + 0;

  /**
   * The meta object id for the '{@link org.eclipse.wazaabi.mm.edp.handlers.State <em>State</em>}' enum.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see org.eclipse.wazaabi.mm.edp.handlers.State
   * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getState()
   * @generated
   */
  int STATE = 15;


  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Action <em>Action</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Action</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Action
   * @generated
   */
  EClass getAction();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Binding <em>Binding</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Binding</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Binding
   * @generated
   */
  EClass getBinding();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Operation <em>Operation</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Operation</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Operation
   * @generated
   */
  EClass getOperation();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.handlers.Operation#isAsync <em>Async</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Async</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Operation#isAsync()
   * @see #getOperation()
   * @generated
   */
  EAttribute getOperation_Async();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Executable <em>Executable</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Executable</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Executable
   * @generated
   */
  EClass getExecutable();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.handlers.Executable#getId <em>Id</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Id</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Executable#getId()
   * @see #getExecutable()
   * @generated
   */
  EAttribute getExecutable_Id();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Sequence <em>Sequence</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Sequence</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Sequence
   * @generated
   */
  EClass getSequence();

  /**
   * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.edp.handlers.Sequence#getExecutables <em>Executables</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Executables</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Sequence#getExecutables()
   * @see #getSequence()
   * @generated
   */
  EReference getSequence_Executables();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Deferred <em>Deferred</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Deferred</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Deferred
   * @generated
   */
  EClass getDeferred();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.handlers.Deferred#getUri <em>Uri</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Uri</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Deferred#getUri()
   * @see #getDeferred()
   * @generated
   */
  EAttribute getDeferred_Uri();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.EventHandler <em>Event Handler</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Event Handler</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.EventHandler
   * @generated
   */
  EClass getEventHandler();

  /**
   * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.edp.handlers.EventHandler#getEvents <em>Events</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Events</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.EventHandler#getEvents()
   * @see #getEventHandler()
   * @generated
   */
  EReference getEventHandler_Events();

  /**
   * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.edp.handlers.EventHandler#getConditions <em>Conditions</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Conditions</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.EventHandler#getConditions()
   * @see #getEventHandler()
   * @generated
   */
  EReference getEventHandler_Conditions();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameterized <em>Parameterized</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Parameterized</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameterized
   * @generated
   */
  EClass getParameterized();

  /**
   * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameterized#getParameters <em>Parameters</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the containment reference list '<em>Parameters</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameterized#getParameters()
   * @see #getParameterized()
   * @generated
   */
  EReference getParameterized_Parameters();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameter <em>Parameter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Parameter</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameter
   * @generated
   */
  EClass getParameter();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameter#getName <em>Name</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Name</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameter#getName()
   * @see #getParameter()
   * @generated
   */
  EAttribute getParameter_Name();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.StringParameter <em>String Parameter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>String Parameter</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.StringParameter
   * @generated
   */
  EClass getStringParameter();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.handlers.StringParameter#getValue <em>Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Value</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.StringParameter#getValue()
   * @see #getStringParameter()
   * @generated
   */
  EAttribute getStringParameter_Value();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.BooleanParameter <em>Boolean Parameter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Boolean Parameter</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.BooleanParameter
   * @generated
   */
  EClass getBooleanParameter();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.handlers.BooleanParameter#isValue <em>Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Value</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.BooleanParameter#isValue()
   * @see #getBooleanParameter()
   * @generated
   */
  EAttribute getBooleanParameter_Value();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.IntParameter <em>Int Parameter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Int Parameter</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.IntParameter
   * @generated
   */
  EClass getIntParameter();

  /**
   * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.edp.handlers.IntParameter#getValue <em>Value</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for the attribute '<em>Value</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.IntParameter#getValue()
   * @see #getIntParameter()
   * @generated
   */
  EAttribute getIntParameter_Value();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Condition <em>Condition</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Condition</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Condition
   * @generated
   */
  EClass getCondition();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Validator <em>Validator</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Validator</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Validator
   * @generated
   */
  EClass getValidator();

  /**
   * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.edp.handlers.Converter <em>Converter</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for class '<em>Converter</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Converter
   * @generated
   */
  EClass getConverter();

  /**
   * Returns the meta object for enum '{@link org.eclipse.wazaabi.mm.edp.handlers.State <em>State</em>}'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the meta object for enum '<em>State</em>'.
   * @see org.eclipse.wazaabi.mm.edp.handlers.State
   * @generated
   */
  EEnum getState();

  /**
   * Returns the factory that creates the instances of the model.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the factory that creates the instances of the model.
   * @generated
   */
  EDPHandlersFactory getEDPHandlersFactory();

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
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ActionImpl <em>Action</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ActionImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getAction()
     * @generated
     */
    EClass ACTION = eINSTANCE.getAction();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.BindingImpl <em>Binding</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.BindingImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getBinding()
     * @generated
     */
    EClass BINDING = eINSTANCE.getBinding();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.OperationImpl <em>Operation</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.OperationImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getOperation()
     * @generated
     */
    EClass OPERATION = eINSTANCE.getOperation();

    /**
     * The meta object literal for the '<em><b>Async</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute OPERATION__ASYNC = eINSTANCE.getOperation_Async();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ExecutableImpl <em>Executable</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ExecutableImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getExecutable()
     * @generated
     */
    EClass EXECUTABLE = eINSTANCE.getExecutable();

    /**
     * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute EXECUTABLE__ID = eINSTANCE.getExecutable_Id();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.SequenceImpl <em>Sequence</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.SequenceImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getSequence()
     * @generated
     */
    EClass SEQUENCE = eINSTANCE.getSequence();

    /**
     * The meta object literal for the '<em><b>Executables</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference SEQUENCE__EXECUTABLES = eINSTANCE.getSequence_Executables();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.DeferredImpl <em>Deferred</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.DeferredImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getDeferred()
     * @generated
     */
    EClass DEFERRED = eINSTANCE.getDeferred();

    /**
     * The meta object literal for the '<em><b>Uri</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute DEFERRED__URI = eINSTANCE.getDeferred_Uri();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl <em>Event Handler</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getEventHandler()
     * @generated
     */
    EClass EVENT_HANDLER = eINSTANCE.getEventHandler();

    /**
     * The meta object literal for the '<em><b>Events</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EVENT_HANDLER__EVENTS = eINSTANCE.getEventHandler_Events();

    /**
     * The meta object literal for the '<em><b>Conditions</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference EVENT_HANDLER__CONDITIONS = eINSTANCE.getEventHandler_Conditions();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameterized <em>Parameterized</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.Parameterized
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getParameterized()
     * @generated
     */
    EClass PARAMETERIZED = eINSTANCE.getParameterized();

    /**
     * The meta object literal for the '<em><b>Parameters</b></em>' containment reference list feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EReference PARAMETERIZED__PARAMETERS = eINSTANCE.getParameterized_Parameters();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameter <em>Parameter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.Parameter
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getParameter()
     * @generated
     */
    EClass PARAMETER = eINSTANCE.getParameter();

    /**
     * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute PARAMETER__NAME = eINSTANCE.getParameter_Name();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.StringParameterImpl <em>String Parameter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.StringParameterImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getStringParameter()
     * @generated
     */
    EClass STRING_PARAMETER = eINSTANCE.getStringParameter();

    /**
     * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute STRING_PARAMETER__VALUE = eINSTANCE.getStringParameter_Value();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.BooleanParameterImpl <em>Boolean Parameter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.BooleanParameterImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getBooleanParameter()
     * @generated
     */
    EClass BOOLEAN_PARAMETER = eINSTANCE.getBooleanParameter();

    /**
     * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute BOOLEAN_PARAMETER__VALUE = eINSTANCE.getBooleanParameter_Value();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.IntParameterImpl <em>Int Parameter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.IntParameterImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getIntParameter()
     * @generated
     */
    EClass INT_PARAMETER = eINSTANCE.getIntParameter();

    /**
     * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    EAttribute INT_PARAMETER__VALUE = eINSTANCE.getIntParameter_Value();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ConditionImpl <em>Condition</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ConditionImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getCondition()
     * @generated
     */
    EClass CONDITION = eINSTANCE.getCondition();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ValidatorImpl <em>Validator</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ValidatorImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getValidator()
     * @generated
     */
    EClass VALIDATOR = eINSTANCE.getValidator();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.impl.ConverterImpl <em>Converter</em>}' class.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.ConverterImpl
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getConverter()
     * @generated
     */
    EClass CONVERTER = eINSTANCE.getConverter();

    /**
     * The meta object literal for the '{@link org.eclipse.wazaabi.mm.edp.handlers.State <em>State</em>}' enum.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see org.eclipse.wazaabi.mm.edp.handlers.State
     * @see org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersPackageImpl#getState()
     * @generated
     */
    EEnum STATE = eINSTANCE.getState();

  }

} //EDPHandlersPackage
