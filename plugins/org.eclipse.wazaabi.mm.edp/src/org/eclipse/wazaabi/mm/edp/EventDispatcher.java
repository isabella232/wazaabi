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
package org.eclipse.wazaabi.mm.edp;

import org.eclipse.emf.common.util.EList;

import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.State;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Event Dispatcher</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.EventDispatcher#getHandlers <em>Handlers</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.EventDispatcher#getState <em>State</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.edp.EdpPackage#getEventDispatcher()
 * @model
 * @generated
 */
public interface EventDispatcher extends Context
{
  /**
   * Returns the value of the '<em><b>Handlers</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.wazaabi.mm.edp.handlers.EventHandler}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Handlers</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Handlers</em>' containment reference list.
   * @see org.eclipse.wazaabi.mm.edp.EdpPackage#getEventDispatcher_Handlers()
   * @model containment="true"
   * @generated
   */
  EList<EventHandler> getHandlers();

  /**
   * Returns the value of the '<em><b>State</b></em>' attribute.
   * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.edp.handlers.State}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>State</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>State</em>' attribute.
   * @see org.eclipse.wazaabi.mm.edp.handlers.State
   * @see #setState(State)
   * @see org.eclipse.wazaabi.mm.edp.EdpPackage#getEventDispatcher_State()
   * @model
   * @generated
   */
  State getState();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.edp.EventDispatcher#getState <em>State</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>State</em>' attribute.
   * @see org.eclipse.wazaabi.mm.edp.handlers.State
   * @see #getState()
   * @generated
   */
  void setState(State value);

} // EventDispatcher
