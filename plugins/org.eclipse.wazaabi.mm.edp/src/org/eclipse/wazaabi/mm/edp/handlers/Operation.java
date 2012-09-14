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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Operation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.Operation#isAsync <em>Async</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#getOperation()
 * @model abstract="true"
 * @generated
 */
public interface Operation extends Deferred, Executable
{
  /**
   * Returns the value of the '<em><b>Async</b></em>' attribute.
   * The default value is <code>"false"</code>.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Async</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Async</em>' attribute.
   * @see #setAsync(boolean)
   * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#getOperation_Async()
   * @model default="false"
   * @generated
   */
  boolean isAsync();

  /**
   * Sets the value of the '{@link org.eclipse.wazaabi.mm.edp.handlers.Operation#isAsync <em>Async</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Async</em>' attribute.
   * @see #isAsync()
   * @generated
   */
  void setAsync(boolean value);

} // Operation
