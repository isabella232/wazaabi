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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.edp.handlers.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class EDPHandlersFactoryImpl extends EFactoryImpl implements EDPHandlersFactory
{
  /**
   * Creates the default factory implementation.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static EDPHandlersFactory init()
  {
    try
    {
      EDPHandlersFactory theEDPHandlersFactory = (EDPHandlersFactory)EPackage.Registry.INSTANCE.getEFactory("http://www.wazaabi.org/edp/handlers"); 
      if (theEDPHandlersFactory != null)
      {
        return theEDPHandlersFactory;
      }
    }
    catch (Exception exception)
    {
      EcorePlugin.INSTANCE.log(exception);
    }
    return new EDPHandlersFactoryImpl();
  }

  /**
   * Creates an instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPHandlersFactoryImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public EObject create(EClass eClass)
  {
    switch (eClass.getClassifierID())
    {
      case EDPHandlersPackage.ACTION: return createAction();
      case EDPHandlersPackage.BINDING: return createBinding();
      case EDPHandlersPackage.EXECUTABLE: return createExecutable();
      case EDPHandlersPackage.SEQUENCE: return createSequence();
      case EDPHandlersPackage.DEFERRED: return createDeferred();
      case EDPHandlersPackage.EVENT_HANDLER: return createEventHandler();
      case EDPHandlersPackage.STRING_PARAMETER: return createStringParameter();
      case EDPHandlersPackage.BOOLEAN_PARAMETER: return createBooleanParameter();
      case EDPHandlersPackage.INT_PARAMETER: return createIntParameter();
      case EDPHandlersPackage.CONDITION: return createCondition();
      case EDPHandlersPackage.VALIDATOR: return createValidator();
      case EDPHandlersPackage.CONVERTER: return createConverter();
      default:
        throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object createFromString(EDataType eDataType, String initialValue)
  {
    switch (eDataType.getClassifierID())
    {
      case EDPHandlersPackage.STATE:
        return createStateFromString(eDataType, initialValue);
      default:
        throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String convertToString(EDataType eDataType, Object instanceValue)
  {
    switch (eDataType.getClassifierID())
    {
      case EDPHandlersPackage.STATE:
        return convertStateToString(eDataType, instanceValue);
      default:
        throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Action createAction()
  {
    ActionImpl action = new ActionImpl();
    return action;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Binding createBinding()
  {
    BindingImpl binding = new BindingImpl();
    return binding;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Executable createExecutable()
  {
    ExecutableImpl executable = new ExecutableImpl();
    return executable;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Sequence createSequence()
  {
    SequenceImpl sequence = new SequenceImpl();
    return sequence;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Deferred createDeferred()
  {
    DeferredImpl deferred = new DeferredImpl();
    return deferred;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EventHandler createEventHandler()
  {
    EventHandlerImpl eventHandler = new EventHandlerImpl();
    return eventHandler;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public StringParameter createStringParameter()
  {
    StringParameterImpl stringParameter = new StringParameterImpl();
    return stringParameter;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public BooleanParameter createBooleanParameter()
  {
    BooleanParameterImpl booleanParameter = new BooleanParameterImpl();
    return booleanParameter;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public IntParameter createIntParameter()
  {
    IntParameterImpl intParameter = new IntParameterImpl();
    return intParameter;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Condition createCondition()
  {
    ConditionImpl condition = new ConditionImpl();
    return condition;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Validator createValidator()
  {
    ValidatorImpl validator = new ValidatorImpl();
    return validator;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Converter createConverter()
  {
    ConverterImpl converter = new ConverterImpl();
    return converter;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public State createStateFromString(EDataType eDataType, String initialValue)
  {
    State result = State.get(initialValue);
    if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
    return result;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String convertStateToString(EDataType eDataType, Object instanceValue)
  {
    return instanceValue == null ? null : instanceValue.toString();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPHandlersPackage getEDPHandlersPackage()
  {
    return (EDPHandlersPackage)getEPackage();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @deprecated
   * @generated
   */
  @Deprecated
  public static EDPHandlersPackage getPackage()
  {
    return EDPHandlersPackage.eINSTANCE;
  }

} //EDPHandlersFactoryImpl
