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

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Sequence</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.Sequence#getExecutables <em>Executables</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#getSequence()
 * @model
 * @generated
 */
public interface Sequence extends Executable
{
  /**
   * Returns the value of the '<em><b>Executables</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.wazaabi.mm.edp.handlers.Executable}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Executables</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Executables</em>' containment reference list.
   * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage#getSequence_Executables()
   * @model containment="true"
   * @generated
   */
  EList<Executable> getExecutables();

} // Sequence
