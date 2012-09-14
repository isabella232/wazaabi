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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Path Event</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.events.PathEvent#getPath <em>Path</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage#getPathEvent()
 * @model abstract="true"
 * @generated
 */
public interface PathEvent extends Event
{
  /**
   * Returns the value of the '<em><b>Path</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Path</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Path</em>' attribute.
   * @see #setPath(String)
   * @see org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage#getPathEvent_Path()
   * @model
   * @generated
   */
  String getPath();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.edp.events.PathEvent#getPath <em>Path</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Path</em>' attribute.
   * @see #getPath()
   * @generated
   */
  void setPath(String value);

} // PathEvent
