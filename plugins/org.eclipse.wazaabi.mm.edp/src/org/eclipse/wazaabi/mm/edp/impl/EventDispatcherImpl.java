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
package org.eclipse.wazaabi.mm.edp.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.wazaabi.mm.edp.ContextContent;
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;

import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.State;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Event Dispatcher</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.impl.EventDispatcherImpl#getContents <em>Contents</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.impl.EventDispatcherImpl#getHandlers <em>Handlers</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.impl.EventDispatcherImpl#getState <em>State</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class EventDispatcherImpl extends EObjectImpl implements EventDispatcher
{
  /**
   * The cached value of the '{@link #getContents() <em>Contents</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getContents()
   * @generated
   * @ordered
   */
  protected EList<ContextContent> contents;

  /**
   * The cached value of the '{@link #getHandlers() <em>Handlers</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getHandlers()
   * @generated
   * @ordered
   */
  protected EList<EventHandler> handlers;

  /**
   * The default value of the '{@link #getState() <em>State</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getState()
   * @generated
   * @ordered
   */
  protected static final State STATE_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getState() <em>State</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getState()
   * @generated
   * @ordered
   */
  protected State state = STATE_EDEFAULT;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected EventDispatcherImpl()
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
    return EdpPackage.Literals.EVENT_DISPATCHER;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ContextContent> getContents()
  {
    if (contents == null)
    {
      contents = new EObjectContainmentEList<ContextContent>(ContextContent.class, this, EdpPackage.EVENT_DISPATCHER__CONTENTS);
    }
    return contents;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<EventHandler> getHandlers()
  {
    if (handlers == null)
    {
      handlers = new EObjectContainmentEList<EventHandler>(EventHandler.class, this, EdpPackage.EVENT_DISPATCHER__HANDLERS);
    }
    return handlers;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public State getState()
  {
    return state;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setState(State newState)
  {
    State oldState = state;
    state = newState == null ? STATE_EDEFAULT : newState;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, EdpPackage.EVENT_DISPATCHER__STATE, oldState, state));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean containsKey(String key)
  {
    return containsKey(key, false);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean containsKey(String key, boolean local)
  {
    if (key == null || "".equals(key)) //$NON-NLS-1$
      return false;
    for (int i = 0; i < getContents().size(); i++) {
      ContextContent content = (ContextContent) getContents().get(i);
      if (key.equals(content.getKey()))
        return true;
    }
    if (!local
        && eContainer() instanceof org.eclipse.wazaabi.mm.edp.Context)
      return ((org.eclipse.wazaabi.mm.edp.Context) eContainer())
          .containsKey(key, false);
    return false;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Object get(String key)
  {
    if (key == null || "".equals(key)) //$NON-NLS-1$
      return null;
    for (int i = 0; i < getContents().size(); i++) {
      ContextContent content = (ContextContent) getContents().get(i);
      if (key.equals(content.getKey()))
        return content.getValue();
    }
    if (eContainer() instanceof org.eclipse.wazaabi.mm.edp.Context)
      return ((org.eclipse.wazaabi.mm.edp.Context) eContainer())
          .get(key);
    return null;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void remove(String key)
  {
    if (key == null || "".equals(key)) //$NON-NLS-1$
      return;
    
    ContextContent content = null;
    boolean found = false;
    for (int i = 0; i < getContents().size(); i++) {
      content = (ContextContent) getContents().get(i);
      if (key.equals(content.getKey())) {
        found = true;
        break;
      }
    }
    if (found)
      getContents().remove(content);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void set(String key, Object value)
  {
    if (key == null || "".equals(key)) //$NON-NLS-1$
      return;
    
    for (int i = 0; i < getContents().size(); i++) {
      ContextContent content = (ContextContent) getContents().get(i);
      if (key.equals(content.getKey())) {
        content.setValue(value);
        return;
      }
    }
    ContextContent newContent = org.eclipse.wazaabi.mm.edp.EdpFactory.eINSTANCE
        .createContextContent();
    newContent.setKey(key);
    newContent.setValue(value);
    getContents().add(newContent);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case EdpPackage.EVENT_DISPATCHER__CONTENTS:
        return ((InternalEList<?>)getContents()).basicRemove(otherEnd, msgs);
      case EdpPackage.EVENT_DISPATCHER__HANDLERS:
        return ((InternalEList<?>)getHandlers()).basicRemove(otherEnd, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case EdpPackage.EVENT_DISPATCHER__CONTENTS:
        return getContents();
      case EdpPackage.EVENT_DISPATCHER__HANDLERS:
        return getHandlers();
      case EdpPackage.EVENT_DISPATCHER__STATE:
        return getState();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case EdpPackage.EVENT_DISPATCHER__CONTENTS:
        getContents().clear();
        getContents().addAll((Collection<? extends ContextContent>)newValue);
        return;
      case EdpPackage.EVENT_DISPATCHER__HANDLERS:
        getHandlers().clear();
        getHandlers().addAll((Collection<? extends EventHandler>)newValue);
        return;
      case EdpPackage.EVENT_DISPATCHER__STATE:
        setState((State)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case EdpPackage.EVENT_DISPATCHER__CONTENTS:
        getContents().clear();
        return;
      case EdpPackage.EVENT_DISPATCHER__HANDLERS:
        getHandlers().clear();
        return;
      case EdpPackage.EVENT_DISPATCHER__STATE:
        setState(STATE_EDEFAULT);
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case EdpPackage.EVENT_DISPATCHER__CONTENTS:
        return contents != null && !contents.isEmpty();
      case EdpPackage.EVENT_DISPATCHER__HANDLERS:
        return handlers != null && !handlers.isEmpty();
      case EdpPackage.EVENT_DISPATCHER__STATE:
        return state != STATE_EDEFAULT;
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public String toString()
  {
    if (eIsProxy()) return super.toString();

    StringBuffer result = new StringBuffer(super.toString());
    result.append(" (state: ");
    result.append(state);
    result.append(')');
    return result.toString();
  }

} //EventDispatcherImpl
