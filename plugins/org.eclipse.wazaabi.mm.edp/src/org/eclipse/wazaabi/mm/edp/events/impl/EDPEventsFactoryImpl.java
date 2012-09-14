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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.edp.events.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class EDPEventsFactoryImpl extends EFactoryImpl implements EDPEventsFactory
{
  /**
   * Creates the default factory implementation.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public static EDPEventsFactory init()
  {
    try
    {
      EDPEventsFactory theEDPEventsFactory = (EDPEventsFactory)EPackage.Registry.INSTANCE.getEFactory("http://www.wazaabi.org/edp/events"); 
      if (theEDPEventsFactory != null)
      {
        return theEDPEventsFactory;
      }
    }
    catch (Exception exception)
    {
      EcorePlugin.INSTANCE.log(exception);
    }
    return new EDPEventsFactoryImpl();
  }

  /**
   * Creates an instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPEventsFactoryImpl()
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
      case EDPEventsPackage.CONTENT_CHANGED_EVENT: return createContentChangedEvent();
      case EDPEventsPackage.EVENT: return createEvent();
      case EDPEventsPackage.PROPERTY_CHANGED_EVENT: return createPropertyChangedEvent();
      default:
        throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
    }
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public ContentChangedEvent createContentChangedEvent()
  {
    ContentChangedEventImpl contentChangedEvent = new ContentChangedEventImpl();
    return contentChangedEvent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Event createEvent()
  {
    EventImpl event = new EventImpl();
    return event;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PropertyChangedEvent createPropertyChangedEvent()
  {
    PropertyChangedEventImpl propertyChangedEvent = new PropertyChangedEventImpl();
    return propertyChangedEvent;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EDPEventsPackage getEDPEventsPackage()
  {
    return (EDPEventsPackage)getEPackage();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @deprecated
   * @generated
   */
  @Deprecated
  public static EDPEventsPackage getPackage()
  {
    return EDPEventsPackage.eINSTANCE;
  }

} //EDPEventsFactoryImpl
