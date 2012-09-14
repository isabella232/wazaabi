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
package org.eclipse.wazaabi.mm.edp.handlers.util;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

import org.eclipse.wazaabi.mm.edp.handlers.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage
 * @generated
 */
public class EDPHandlersSwitch<T> extends Switch<T>
{
  /**
   * The cached model package
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static EDPHandlersPackage modelPackage;

  /**
   * Creates an instance of the switch.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPHandlersSwitch()
  {
    if (modelPackage == null)
    {
      modelPackage = EDPHandlersPackage.eINSTANCE;
    }
  }

  /**
   * Checks whether this is a switch for the given package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @parameter ePackage the package in question.
   * @return whether this is a switch for the given package.
   * @generated
   */
  @Override
  protected boolean isSwitchFor(EPackage ePackage)
  {
    return ePackage == modelPackage;
  }

  /**
   * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the first non-null result returned by a <code>caseXXX</code> call.
   * @generated
   */
  @Override
  protected T doSwitch(int classifierID, EObject theEObject)
  {
    switch (classifierID)
    {
      case EDPHandlersPackage.ACTION:
      {
        Action action = (Action)theEObject;
        T result = caseAction(action);
        if (result == null) result = caseOperation(action);
        if (result == null) result = caseDeferred(action);
        if (result == null) result = caseExecutable(action);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.BINDING:
      {
        Binding binding = (Binding)theEObject;
        T result = caseBinding(binding);
        if (result == null) result = caseEventHandler(binding);
        if (result == null) result = caseParameterized(binding);
        if (result == null) result = caseSequence(binding);
        if (result == null) result = caseAction(binding);
        if (result == null) result = caseOperation(binding);
        if (result == null) result = caseExecutable(binding);
        if (result == null) result = caseDeferred(binding);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.OPERATION:
      {
        Operation operation = (Operation)theEObject;
        T result = caseOperation(operation);
        if (result == null) result = caseDeferred(operation);
        if (result == null) result = caseExecutable(operation);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.EXECUTABLE:
      {
        Executable executable = (Executable)theEObject;
        T result = caseExecutable(executable);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.SEQUENCE:
      {
        Sequence sequence = (Sequence)theEObject;
        T result = caseSequence(sequence);
        if (result == null) result = caseExecutable(sequence);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.DEFERRED:
      {
        Deferred deferred = (Deferred)theEObject;
        T result = caseDeferred(deferred);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.EVENT_HANDLER:
      {
        EventHandler eventHandler = (EventHandler)theEObject;
        T result = caseEventHandler(eventHandler);
        if (result == null) result = caseParameterized(eventHandler);
        if (result == null) result = caseSequence(eventHandler);
        if (result == null) result = caseAction(eventHandler);
        if (result == null) result = caseOperation(eventHandler);
        if (result == null) result = caseExecutable(eventHandler);
        if (result == null) result = caseDeferred(eventHandler);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.PARAMETERIZED:
      {
        Parameterized parameterized = (Parameterized)theEObject;
        T result = caseParameterized(parameterized);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.PARAMETER:
      {
        Parameter parameter = (Parameter)theEObject;
        T result = caseParameter(parameter);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.STRING_PARAMETER:
      {
        StringParameter stringParameter = (StringParameter)theEObject;
        T result = caseStringParameter(stringParameter);
        if (result == null) result = caseParameter(stringParameter);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.BOOLEAN_PARAMETER:
      {
        BooleanParameter booleanParameter = (BooleanParameter)theEObject;
        T result = caseBooleanParameter(booleanParameter);
        if (result == null) result = caseParameter(booleanParameter);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.INT_PARAMETER:
      {
        IntParameter intParameter = (IntParameter)theEObject;
        T result = caseIntParameter(intParameter);
        if (result == null) result = caseParameter(intParameter);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.CONDITION:
      {
        Condition condition = (Condition)theEObject;
        T result = caseCondition(condition);
        if (result == null) result = caseValidator(condition);
        if (result == null) result = caseOperation(condition);
        if (result == null) result = caseDeferred(condition);
        if (result == null) result = caseExecutable(condition);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.VALIDATOR:
      {
        Validator validator = (Validator)theEObject;
        T result = caseValidator(validator);
        if (result == null) result = caseOperation(validator);
        if (result == null) result = caseDeferred(validator);
        if (result == null) result = caseExecutable(validator);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      case EDPHandlersPackage.CONVERTER:
      {
        Converter converter = (Converter)theEObject;
        T result = caseConverter(converter);
        if (result == null) result = caseAction(converter);
        if (result == null) result = caseOperation(converter);
        if (result == null) result = caseDeferred(converter);
        if (result == null) result = caseExecutable(converter);
        if (result == null) result = defaultCase(theEObject);
        return result;
      }
      default: return defaultCase(theEObject);
    }
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Action</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Action</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseAction(Action object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Binding</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Binding</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseBinding(Binding object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Operation</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Operation</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseOperation(Operation object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Executable</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Executable</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseExecutable(Executable object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Sequence</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Sequence</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseSequence(Sequence object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Deferred</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Deferred</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseDeferred(Deferred object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Event Handler</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Event Handler</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseEventHandler(EventHandler object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Parameterized</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Parameterized</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseParameterized(Parameterized object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Parameter</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Parameter</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseParameter(Parameter object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>String Parameter</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>String Parameter</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseStringParameter(StringParameter object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Boolean Parameter</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Boolean Parameter</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseBooleanParameter(BooleanParameter object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Int Parameter</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Int Parameter</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseIntParameter(IntParameter object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Condition</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Condition</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseCondition(Condition object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Validator</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Validator</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseValidator(Validator object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>Converter</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>Converter</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
   * @generated
   */
  public T caseConverter(Converter object)
  {
    return null;
  }

  /**
   * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
   * <!-- begin-user-doc -->
   * This implementation returns null;
   * returning a non-null result will terminate the switch, but this is the last case anyway.
   * <!-- end-user-doc -->
   * @param object the target of the switch.
   * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
   * @see #doSwitch(org.eclipse.emf.ecore.EObject)
   * @generated
   */
  @Override
  public T defaultCase(EObject object)
  {
    return null;
  }

} //EDPHandlersSwitch
