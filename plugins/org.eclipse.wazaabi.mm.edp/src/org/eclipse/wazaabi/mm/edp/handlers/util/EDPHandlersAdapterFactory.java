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

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.wazaabi.mm.edp.handlers.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage
 * @generated
 */
public class EDPHandlersAdapterFactory extends AdapterFactoryImpl
{
  /**
   * The cached model package.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected static EDPHandlersPackage modelPackage;

  /**
   * Creates an instance of the adapter factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPHandlersAdapterFactory()
  {
    if (modelPackage == null)
    {
      modelPackage = EDPHandlersPackage.eINSTANCE;
    }
  }

  /**
   * Returns whether this factory is applicable for the type of the object.
   * <!-- begin-user-doc -->
   * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
   * <!-- end-user-doc -->
   * @return whether this factory is applicable for the type of the object.
   * @generated
   */
  @Override
  public boolean isFactoryForType(Object object)
  {
    if (object == modelPackage)
    {
      return true;
    }
    if (object instanceof EObject)
    {
      return ((EObject)object).eClass().getEPackage() == modelPackage;
    }
    return false;
  }

  /**
   * The switch that delegates to the <code>createXXX</code> methods.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected EDPHandlersSwitch<Adapter> modelSwitch =
    new EDPHandlersSwitch<Adapter>()
    {
      @Override
      public Adapter caseAction(Action object)
      {
        return createActionAdapter();
      }
      @Override
      public Adapter caseBinding(Binding object)
      {
        return createBindingAdapter();
      }
      @Override
      public Adapter caseOperation(Operation object)
      {
        return createOperationAdapter();
      }
      @Override
      public Adapter caseExecutable(Executable object)
      {
        return createExecutableAdapter();
      }
      @Override
      public Adapter caseSequence(Sequence object)
      {
        return createSequenceAdapter();
      }
      @Override
      public Adapter caseDeferred(Deferred object)
      {
        return createDeferredAdapter();
      }
      @Override
      public Adapter caseEventHandler(EventHandler object)
      {
        return createEventHandlerAdapter();
      }
      @Override
      public Adapter caseParameterized(Parameterized object)
      {
        return createParameterizedAdapter();
      }
      @Override
      public Adapter caseParameter(Parameter object)
      {
        return createParameterAdapter();
      }
      @Override
      public Adapter caseStringParameter(StringParameter object)
      {
        return createStringParameterAdapter();
      }
      @Override
      public Adapter caseBooleanParameter(BooleanParameter object)
      {
        return createBooleanParameterAdapter();
      }
      @Override
      public Adapter caseIntParameter(IntParameter object)
      {
        return createIntParameterAdapter();
      }
      @Override
      public Adapter caseCondition(Condition object)
      {
        return createConditionAdapter();
      }
      @Override
      public Adapter caseValidator(Validator object)
      {
        return createValidatorAdapter();
      }
      @Override
      public Adapter caseConverter(Converter object)
      {
        return createConverterAdapter();
      }
      @Override
      public Adapter defaultCase(EObject object)
      {
        return createEObjectAdapter();
      }
    };

  /**
   * Creates an adapter for the <code>target</code>.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param target the object to adapt.
   * @return the adapter for the <code>target</code>.
   * @generated
   */
  @Override
  public Adapter createAdapter(Notifier target)
  {
    return modelSwitch.doSwitch((EObject)target);
  }


  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Action <em>Action</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Action
   * @generated
   */
  public Adapter createActionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Binding <em>Binding</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Binding
   * @generated
   */
  public Adapter createBindingAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Operation <em>Operation</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Operation
   * @generated
   */
  public Adapter createOperationAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Executable <em>Executable</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Executable
   * @generated
   */
  public Adapter createExecutableAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Sequence <em>Sequence</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Sequence
   * @generated
   */
  public Adapter createSequenceAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Deferred <em>Deferred</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Deferred
   * @generated
   */
  public Adapter createDeferredAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.EventHandler <em>Event Handler</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.EventHandler
   * @generated
   */
  public Adapter createEventHandlerAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameterized <em>Parameterized</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameterized
   * @generated
   */
  public Adapter createParameterizedAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameter <em>Parameter</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Parameter
   * @generated
   */
  public Adapter createParameterAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.StringParameter <em>String Parameter</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.StringParameter
   * @generated
   */
  public Adapter createStringParameterAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.BooleanParameter <em>Boolean Parameter</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.BooleanParameter
   * @generated
   */
  public Adapter createBooleanParameterAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.IntParameter <em>Int Parameter</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.IntParameter
   * @generated
   */
  public Adapter createIntParameterAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Condition <em>Condition</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Condition
   * @generated
   */
  public Adapter createConditionAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Validator <em>Validator</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Validator
   * @generated
   */
  public Adapter createValidatorAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Converter <em>Converter</em>}'.
   * <!-- begin-user-doc -->
   * This default implementation returns null so that we can easily ignore cases;
   * it's useful to ignore a case when inheritance will catch all the cases anyway.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @see org.eclipse.wazaabi.mm.edp.handlers.Converter
   * @generated
   */
  public Adapter createConverterAdapter()
  {
    return null;
  }

  /**
   * Creates a new adapter for the default case.
   * <!-- begin-user-doc -->
   * This default implementation returns null.
   * <!-- end-user-doc -->
   * @return the new adapter.
   * @generated
   */
  public Adapter createEObjectAdapter()
  {
    return null;
  }

} //EDPHandlersAdapterFactory
