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
package org.eclipse.wazaabi.mm.core.widgets.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Collection</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.CollectionImpl#getInput <em>Input</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.CollectionImpl#getSelection <em>Selection</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.CollectionImpl#getCheckedElements <em>Checked Elements</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class CollectionImpl extends AbstractComponentImpl implements Collection {
	/**
     * The default value of the '{@link #getInput() <em>Input</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getInput()
     * @generated
     * @ordered
     */
	protected static final Object INPUT_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getInput() <em>Input</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getInput()
     * @generated
     * @ordered
     */
	protected Object input = INPUT_EDEFAULT;

	/**
     * The cached value of the '{@link #getSelection() <em>Selection</em>}' attribute list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getSelection()
     * @generated
     * @ordered
     */
	protected EList<Object> selection;

	/**
     * The cached value of the '{@link #getCheckedElements() <em>Checked Elements</em>}' attribute list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getCheckedElements()
     * @generated
     * @ordered
     */
	protected EList<Object> checkedElements;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected CollectionImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreWidgetsPackage.Literals.COLLECTION;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Object> getSelection() {
        if (selection == null) {
            selection = new EDataTypeUniqueEList<Object>(Object.class, this, CoreWidgetsPackage.COLLECTION__SELECTION);
        }
        return selection;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Object> getCheckedElements() {
        if (checkedElements == null) {
            checkedElements = new EDataTypeUniqueEList<Object>(Object.class, this, CoreWidgetsPackage.COLLECTION__CHECKED_ELEMENTS);
        }
        return checkedElements;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Object getInput() {
        return input;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setInput(Object newInput) {
        Object oldInput = input;
        input = newInput;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.COLLECTION__INPUT, oldInput, input));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case CoreWidgetsPackage.COLLECTION__INPUT:
                return getInput();
            case CoreWidgetsPackage.COLLECTION__SELECTION:
                return getSelection();
            case CoreWidgetsPackage.COLLECTION__CHECKED_ELEMENTS:
                return getCheckedElements();
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
	public void eSet(int featureID, Object newValue) {
        switch (featureID) {
            case CoreWidgetsPackage.COLLECTION__INPUT:
                setInput(newValue);
                return;
            case CoreWidgetsPackage.COLLECTION__SELECTION:
                getSelection().clear();
                getSelection().addAll((java.util.Collection<? extends Object>)newValue);
                return;
            case CoreWidgetsPackage.COLLECTION__CHECKED_ELEMENTS:
                getCheckedElements().clear();
                getCheckedElements().addAll((java.util.Collection<? extends Object>)newValue);
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
	public void eUnset(int featureID) {
        switch (featureID) {
            case CoreWidgetsPackage.COLLECTION__INPUT:
                setInput(INPUT_EDEFAULT);
                return;
            case CoreWidgetsPackage.COLLECTION__SELECTION:
                getSelection().clear();
                return;
            case CoreWidgetsPackage.COLLECTION__CHECKED_ELEMENTS:
                getCheckedElements().clear();
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
	public boolean eIsSet(int featureID) {
        switch (featureID) {
            case CoreWidgetsPackage.COLLECTION__INPUT:
                return INPUT_EDEFAULT == null ? input != null : !INPUT_EDEFAULT.equals(input);
            case CoreWidgetsPackage.COLLECTION__SELECTION:
                return selection != null && !selection.isEmpty();
            case CoreWidgetsPackage.COLLECTION__CHECKED_ELEMENTS:
                return checkedElements != null && !checkedElements.isEmpty();
        }
        return super.eIsSet(featureID);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public String toString() {
        if (eIsProxy()) return super.toString();

        StringBuffer result = new StringBuffer(super.toString());
        result.append(" (input: ");
        result.append(input);
        result.append(", selection: ");
        result.append(selection);
        result.append(", checkedElements: ");
        result.append(checkedElements);
        result.append(')');
        return result.toString();
    }

} //CollectionImpl
