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
package org.eclipse.wazaabi.mm.edp.handlers.impl;

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

import org.eclipse.wazaabi.mm.edp.events.Event;

import org.eclipse.wazaabi.mm.edp.handlers.Action;
import org.eclipse.wazaabi.mm.edp.handlers.Condition;
import org.eclipse.wazaabi.mm.edp.handlers.Deferred;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Executable;
import org.eclipse.wazaabi.mm.edp.handlers.Operation;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Sequence;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Event Handler</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl#getParameters <em>Parameters</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl#getId <em>Id</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl#getExecutables <em>Executables</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl#getUri <em>Uri</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl#isAsync <em>Async</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl#getEvents <em>Events</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.handlers.impl.EventHandlerImpl#getConditions <em>Conditions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class EventHandlerImpl extends EObjectImpl implements EventHandler
{
  /**
   * The cached value of the '{@link #getParameters() <em>Parameters</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getParameters()
   * @generated
   * @ordered
   */
  protected EList<Parameter> parameters;

  /**
   * The default value of the '{@link #getId() <em>Id</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getId()
   * @generated
   * @ordered
   */
  protected static final String ID_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getId() <em>Id</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getId()
   * @generated
   * @ordered
   */
  protected String id = ID_EDEFAULT;

  /**
   * The cached value of the '{@link #getExecutables() <em>Executables</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExecutables()
   * @generated
   * @ordered
   */
  protected EList<Executable> executables;

  /**
   * The default value of the '{@link #getUri() <em>Uri</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getUri()
   * @generated
   * @ordered
   */
  protected static final String URI_EDEFAULT = null;

  /**
   * The cached value of the '{@link #getUri() <em>Uri</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getUri()
   * @generated
   * @ordered
   */
  protected String uri = URI_EDEFAULT;

  /**
   * The default value of the '{@link #isAsync() <em>Async</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isAsync()
   * @generated
   * @ordered
   */
  protected static final boolean ASYNC_EDEFAULT = false;

  /**
   * The cached value of the '{@link #isAsync() <em>Async</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #isAsync()
   * @generated
   * @ordered
   */
  protected boolean async = ASYNC_EDEFAULT;

  /**
   * The cached value of the '{@link #getEvents() <em>Events</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getEvents()
   * @generated
   * @ordered
   */
  protected EList<Event> events;

  /**
   * The cached value of the '{@link #getConditions() <em>Conditions</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getConditions()
   * @generated
   * @ordered
   */
  protected EList<Condition> conditions;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected EventHandlerImpl()
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
    return EDPHandlersPackage.Literals.EVENT_HANDLER;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Parameter> getParameters()
  {
    if (parameters == null)
    {
      parameters = new EObjectContainmentEList<Parameter>(Parameter.class, this, EDPHandlersPackage.EVENT_HANDLER__PARAMETERS);
    }
    return parameters;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getId()
  {
    return id;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setId(String newId)
  {
    String oldId = id;
    id = newId;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, EDPHandlersPackage.EVENT_HANDLER__ID, oldId, id));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Executable> getExecutables()
  {
    if (executables == null)
    {
      executables = new EObjectContainmentEList<Executable>(Executable.class, this, EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES);
    }
    return executables;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public String getUri()
  {
    return uri;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setUri(String newUri)
  {
    String oldUri = uri;
    uri = newUri;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, EDPHandlersPackage.EVENT_HANDLER__URI, oldUri, uri));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public boolean isAsync()
  {
    return async;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAsync(boolean newAsync)
  {
    boolean oldAsync = async;
    async = newAsync;
    if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, EDPHandlersPackage.EVENT_HANDLER__ASYNC, oldAsync, async));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Event> getEvents()
  {
    if (events == null)
    {
      events = new EObjectContainmentEList<Event>(Event.class, this, EDPHandlersPackage.EVENT_HANDLER__EVENTS);
    }
    return events;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Condition> getConditions()
  {
    if (conditions == null)
    {
      conditions = new EObjectContainmentEList<Condition>(Condition.class, this, EDPHandlersPackage.EVENT_HANDLER__CONDITIONS);
    }
    return conditions;
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
      case EDPHandlersPackage.EVENT_HANDLER__PARAMETERS:
        return ((InternalEList<?>)getParameters()).basicRemove(otherEnd, msgs);
      case EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES:
        return ((InternalEList<?>)getExecutables()).basicRemove(otherEnd, msgs);
      case EDPHandlersPackage.EVENT_HANDLER__EVENTS:
        return ((InternalEList<?>)getEvents()).basicRemove(otherEnd, msgs);
      case EDPHandlersPackage.EVENT_HANDLER__CONDITIONS:
        return ((InternalEList<?>)getConditions()).basicRemove(otherEnd, msgs);
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
      case EDPHandlersPackage.EVENT_HANDLER__PARAMETERS:
        return getParameters();
      case EDPHandlersPackage.EVENT_HANDLER__ID:
        return getId();
      case EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES:
        return getExecutables();
      case EDPHandlersPackage.EVENT_HANDLER__URI:
        return getUri();
      case EDPHandlersPackage.EVENT_HANDLER__ASYNC:
        return isAsync();
      case EDPHandlersPackage.EVENT_HANDLER__EVENTS:
        return getEvents();
      case EDPHandlersPackage.EVENT_HANDLER__CONDITIONS:
        return getConditions();
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
      case EDPHandlersPackage.EVENT_HANDLER__PARAMETERS:
        getParameters().clear();
        getParameters().addAll((Collection<? extends Parameter>)newValue);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__ID:
        setId((String)newValue);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES:
        getExecutables().clear();
        getExecutables().addAll((Collection<? extends Executable>)newValue);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__URI:
        setUri((String)newValue);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__ASYNC:
        setAsync((Boolean)newValue);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__EVENTS:
        getEvents().clear();
        getEvents().addAll((Collection<? extends Event>)newValue);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__CONDITIONS:
        getConditions().clear();
        getConditions().addAll((Collection<? extends Condition>)newValue);
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
      case EDPHandlersPackage.EVENT_HANDLER__PARAMETERS:
        getParameters().clear();
        return;
      case EDPHandlersPackage.EVENT_HANDLER__ID:
        setId(ID_EDEFAULT);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES:
        getExecutables().clear();
        return;
      case EDPHandlersPackage.EVENT_HANDLER__URI:
        setUri(URI_EDEFAULT);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__ASYNC:
        setAsync(ASYNC_EDEFAULT);
        return;
      case EDPHandlersPackage.EVENT_HANDLER__EVENTS:
        getEvents().clear();
        return;
      case EDPHandlersPackage.EVENT_HANDLER__CONDITIONS:
        getConditions().clear();
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
      case EDPHandlersPackage.EVENT_HANDLER__PARAMETERS:
        return parameters != null && !parameters.isEmpty();
      case EDPHandlersPackage.EVENT_HANDLER__ID:
        return ID_EDEFAULT == null ? id != null : !ID_EDEFAULT.equals(id);
      case EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES:
        return executables != null && !executables.isEmpty();
      case EDPHandlersPackage.EVENT_HANDLER__URI:
        return URI_EDEFAULT == null ? uri != null : !URI_EDEFAULT.equals(uri);
      case EDPHandlersPackage.EVENT_HANDLER__ASYNC:
        return async != ASYNC_EDEFAULT;
      case EDPHandlersPackage.EVENT_HANDLER__EVENTS:
        return events != null && !events.isEmpty();
      case EDPHandlersPackage.EVENT_HANDLER__CONDITIONS:
        return conditions != null && !conditions.isEmpty();
    }
    return super.eIsSet(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public int eBaseStructuralFeatureID(int derivedFeatureID, Class<?> baseClass)
  {
    if (baseClass == Executable.class)
    {
      switch (derivedFeatureID)
      {
        case EDPHandlersPackage.EVENT_HANDLER__ID: return EDPHandlersPackage.EXECUTABLE__ID;
        default: return -1;
      }
    }
    if (baseClass == Sequence.class)
    {
      switch (derivedFeatureID)
      {
        case EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES: return EDPHandlersPackage.SEQUENCE__EXECUTABLES;
        default: return -1;
      }
    }
    if (baseClass == Deferred.class)
    {
      switch (derivedFeatureID)
      {
        case EDPHandlersPackage.EVENT_HANDLER__URI: return EDPHandlersPackage.DEFERRED__URI;
        default: return -1;
      }
    }
    if (baseClass == Operation.class)
    {
      switch (derivedFeatureID)
      {
        case EDPHandlersPackage.EVENT_HANDLER__ASYNC: return EDPHandlersPackage.OPERATION__ASYNC;
        default: return -1;
      }
    }
    if (baseClass == Action.class)
    {
      switch (derivedFeatureID)
      {
        default: return -1;
      }
    }
    return super.eBaseStructuralFeatureID(derivedFeatureID, baseClass);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public int eDerivedStructuralFeatureID(int baseFeatureID, Class<?> baseClass)
  {
    if (baseClass == Executable.class)
    {
      switch (baseFeatureID)
      {
        case EDPHandlersPackage.EXECUTABLE__ID: return EDPHandlersPackage.EVENT_HANDLER__ID;
        default: return -1;
      }
    }
    if (baseClass == Sequence.class)
    {
      switch (baseFeatureID)
      {
        case EDPHandlersPackage.SEQUENCE__EXECUTABLES: return EDPHandlersPackage.EVENT_HANDLER__EXECUTABLES;
        default: return -1;
      }
    }
    if (baseClass == Deferred.class)
    {
      switch (baseFeatureID)
      {
        case EDPHandlersPackage.DEFERRED__URI: return EDPHandlersPackage.EVENT_HANDLER__URI;
        default: return -1;
      }
    }
    if (baseClass == Operation.class)
    {
      switch (baseFeatureID)
      {
        case EDPHandlersPackage.OPERATION__ASYNC: return EDPHandlersPackage.EVENT_HANDLER__ASYNC;
        default: return -1;
      }
    }
    if (baseClass == Action.class)
    {
      switch (baseFeatureID)
      {
        default: return -1;
      }
    }
    return super.eDerivedStructuralFeatureID(baseFeatureID, baseClass);
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
    result.append(" (id: ");
    result.append(id);
    result.append(", uri: ");
    result.append(uri);
    result.append(", async: ");
    result.append(async);
    result.append(')');
    return result.toString();
  }

} //EventHandlerImpl
