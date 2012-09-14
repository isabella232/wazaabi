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

import org.eclipse.wazaabi.mm.edp.events.ContentChangedEvent;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Content Changed Event</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * </p>
 *
 * @generated
 */
public class ContentChangedEventImpl extends PathEventImpl implements ContentChangedEvent
{
  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ContentChangedEventImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return EDPEventsPackage.Literals.CONTENT_CHANGED_EVENT;
  }

} //ContentChangedEventImpl
