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
package org.eclipse.wazaabi.mm.core.styles.collections.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Path Selector</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl#getEClassifierName <em>EClassifier Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl#getContext <em>Context</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.PathSelectorImpl#getPaths <em>Paths</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PathSelectorImpl extends EObjectImpl implements PathSelector {
	/**
     * The default value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyName()
     * @generated
     * @ordered
     */
	protected static final String PROPERTY_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyName()
     * @generated
     * @ordered
     */
	protected String propertyName = PROPERTY_NAME_EDEFAULT;

	/**
     * The default value of the '{@link #getEClassifierName() <em>EClassifier Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getEClassifierName()
     * @generated
     * @ordered
     */
	protected static final String ECLASSIFIER_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getEClassifierName() <em>EClassifier Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getEClassifierName()
     * @generated
     * @ordered
     */
	protected String eClassifierName = ECLASSIFIER_NAME_EDEFAULT;

	/**
     * The default value of the '{@link #getContext() <em>Context</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getContext()
     * @generated
     * @ordered
     */
	protected static final String CONTEXT_EDEFAULT = ".";

	/**
     * The cached value of the '{@link #getContext() <em>Context</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getContext()
     * @generated
     * @ordered
     */
	protected String context = CONTEXT_EDEFAULT;

	/**
     * The cached value of the '{@link #getPaths() <em>Paths</em>}' attribute list.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPaths()
     * @generated
     * @ordered
     */
	protected EList<String> paths;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected PathSelectorImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreCollectionsStylesPackage.Literals.PATH_SELECTOR;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getPropertyName() {
        return propertyName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setPropertyName(String newPropertyName) {
        String oldPropertyName = propertyName;
        propertyName = newPropertyName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.PATH_SELECTOR__PROPERTY_NAME, oldPropertyName, propertyName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getEClassifierName() {
        return eClassifierName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setEClassifierName(String newEClassifierName) {
        String oldEClassifierName = eClassifierName;
        eClassifierName = newEClassifierName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.PATH_SELECTOR__ECLASSIFIER_NAME, oldEClassifierName, eClassifierName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getContext() {
        return context;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setContext(String newContext) {
        String oldContext = context;
        context = newContext;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.PATH_SELECTOR__CONTEXT, oldContext, context));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<String> getPaths() {
        if (paths == null) {
            paths = new EDataTypeUniqueEList<String>(String.class, this, CoreCollectionsStylesPackage.PATH_SELECTOR__PATHS);
        }
        return paths;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PROPERTY_NAME:
                return getPropertyName();
            case CoreCollectionsStylesPackage.PATH_SELECTOR__ECLASSIFIER_NAME:
                return getEClassifierName();
            case CoreCollectionsStylesPackage.PATH_SELECTOR__CONTEXT:
                return getContext();
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PATHS:
                return getPaths();
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
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PROPERTY_NAME:
                setPropertyName((String)newValue);
                return;
            case CoreCollectionsStylesPackage.PATH_SELECTOR__ECLASSIFIER_NAME:
                setEClassifierName((String)newValue);
                return;
            case CoreCollectionsStylesPackage.PATH_SELECTOR__CONTEXT:
                setContext((String)newValue);
                return;
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PATHS:
                getPaths().clear();
                getPaths().addAll((Collection<? extends String>)newValue);
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
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PROPERTY_NAME:
                setPropertyName(PROPERTY_NAME_EDEFAULT);
                return;
            case CoreCollectionsStylesPackage.PATH_SELECTOR__ECLASSIFIER_NAME:
                setEClassifierName(ECLASSIFIER_NAME_EDEFAULT);
                return;
            case CoreCollectionsStylesPackage.PATH_SELECTOR__CONTEXT:
                setContext(CONTEXT_EDEFAULT);
                return;
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PATHS:
                getPaths().clear();
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
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PROPERTY_NAME:
                return PROPERTY_NAME_EDEFAULT == null ? propertyName != null : !PROPERTY_NAME_EDEFAULT.equals(propertyName);
            case CoreCollectionsStylesPackage.PATH_SELECTOR__ECLASSIFIER_NAME:
                return ECLASSIFIER_NAME_EDEFAULT == null ? eClassifierName != null : !ECLASSIFIER_NAME_EDEFAULT.equals(eClassifierName);
            case CoreCollectionsStylesPackage.PATH_SELECTOR__CONTEXT:
                return CONTEXT_EDEFAULT == null ? context != null : !CONTEXT_EDEFAULT.equals(context);
            case CoreCollectionsStylesPackage.PATH_SELECTOR__PATHS:
                return paths != null && !paths.isEmpty();
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
        result.append(" (propertyName: ");
        result.append(propertyName);
        result.append(", eClassifierName: ");
        result.append(eClassifierName);
        result.append(", context: ");
        result.append(context);
        result.append(", paths: ");
        result.append(paths);
        result.append(')');
        return result.toString();
    }

} //PathSelectorImpl
