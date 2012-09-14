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

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage
 * @generated
 */
public interface EDPHandlersFactory extends EFactory
{
  /**
   * The singleton instance of the factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  EDPHandlersFactory eINSTANCE = org.eclipse.wazaabi.mm.edp.handlers.impl.EDPHandlersFactoryImpl.init();

  /**
   * Returns a new object of class '<em>Action</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Action</em>'.
   * @generated
   */
  Action createAction();

  /**
   * Returns a new object of class '<em>Binding</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Binding</em>'.
   * @generated
   */
  Binding createBinding();

  /**
   * Returns a new object of class '<em>Executable</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Executable</em>'.
   * @generated
   */
  Executable createExecutable();

  /**
   * Returns a new object of class '<em>Sequence</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Sequence</em>'.
   * @generated
   */
  Sequence createSequence();

  /**
   * Returns a new object of class '<em>Deferred</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Deferred</em>'.
   * @generated
   */
  Deferred createDeferred();

  /**
   * Returns a new object of class '<em>Event Handler</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Event Handler</em>'.
   * @generated
   */
  EventHandler createEventHandler();

  /**
   * Returns a new object of class '<em>String Parameter</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>String Parameter</em>'.
   * @generated
   */
  StringParameter createStringParameter();

  /**
   * Returns a new object of class '<em>Boolean Parameter</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Boolean Parameter</em>'.
   * @generated
   */
  BooleanParameter createBooleanParameter();

  /**
   * Returns a new object of class '<em>Int Parameter</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Int Parameter</em>'.
   * @generated
   */
  IntParameter createIntParameter();

  /**
   * Returns a new object of class '<em>Condition</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Condition</em>'.
   * @generated
   */
  Condition createCondition();

  /**
   * Returns a new object of class '<em>Validator</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Validator</em>'.
   * @generated
   */
  Validator createValidator();

  /**
   * Returns a new object of class '<em>Converter</em>'.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return a new object of class '<em>Converter</em>'.
   * @generated
   */
  Converter createConverter();

  /**
   * Returns the package supported by this factory.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @return the package supported by this factory.
   * @generated
   */
  EDPHandlersPackage getEDPHandlersPackage();

} //EDPHandlersFactory
