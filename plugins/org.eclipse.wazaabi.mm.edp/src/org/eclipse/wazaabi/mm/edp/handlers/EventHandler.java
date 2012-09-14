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

import org.eclipse.emf.common.util.EList;

import org.eclipse.wazaabi.mm.edp.events.Event;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event Handler</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.EventHandler#getEvents <em>Events</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.EventHandler#getConditions <em>Conditions</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#getEventHandler()
 * @model
 * @generated
 */
public interface EventHandler extends Parameterized, Sequence, Action
{
  /**
   * Returns the value of the '<em><b>Events</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.wazaabi.mm.edp.events.Event}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Events</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Events</em>' containment reference list.
   * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#getEventHandler_Events()
   * @model containment="true"
   * @generated
   */
  EList<Event> getEvents();

  /**
   * Returns the value of the '<em><b>Conditions</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.wazaabi.mm.edp.handlers.Condition}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Conditions</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Conditions</em>' containment reference list.
   * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#getEventHandler_Conditions()
   * @model containment="true"
   * @generated
   */
  EList<Condition> getConditions();

} // EventHandler
