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

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage
 * @generated
 */
public interface EDPEventsFactory extends EFactory
{
  /**
   * The singleton instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  EDPEventsFactory eINSTANCE = org.eclipse.wazaabi.mm.edp.events.impl.EDPEventsFactoryImpl.init();

  /**
   * Returns a new object of class '<em>Content Changed Event</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Content Changed Event</em>'.
   * @generated
   */
  ContentChangedEvent createContentChangedEvent();

  /**
   * Returns a new object of class '<em>Event</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Event</em>'.
   * @generated
   */
  Event createEvent();

  /**
   * Returns a new object of class '<em>Property Changed Event</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Property Changed Event</em>'.
   * @generated
   */
  PropertyChangedEvent createPropertyChangedEvent();

  /**
   * Returns the package supported by this factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the package supported by this factory.
   * @generated
   */
  EDPEventsPackage getEDPEventsPackage();

} //EDPEventsFactory
