/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ui.model.parts.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.widgets.impl.ContainerImpl;

import org.eclipse.wazaabi.ui.model.parts.Page;
import org.eclipse.wazaabi.ui.model.parts.PartsPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Page</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.ui.model.parts.impl.PageImpl#getUiSelector <em>Ui Selector</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.ui.model.parts.impl.PageImpl#getSelectionProcessor <em>Selection Processor</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class PageImpl extends ContainerImpl implements Page {
	/**
	 * The default value of the '{@link #getUiSelector() <em>Ui Selector</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUiSelector()
	 * @generated
	 * @ordered
	 */
	protected static final String UI_SELECTOR_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUiSelector() <em>Ui Selector</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUiSelector()
	 * @generated
	 * @ordered
	 */
	protected String uiSelector = UI_SELECTOR_EDEFAULT;

	/**
	 * The default value of the '{@link #getSelectionProcessor() <em>Selection Processor</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSelectionProcessor()
	 * @generated
	 * @ordered
	 */
	protected static final String SELECTION_PROCESSOR_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSelectionProcessor() <em>Selection Processor</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSelectionProcessor()
	 * @generated
	 * @ordered
	 */
	protected String selectionProcessor = SELECTION_PROCESSOR_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected PageImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return PartsPackage.Literals.PAGE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getUiSelector() {
		return uiSelector;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setUiSelector(String newUiSelector) {
		String oldUiSelector = uiSelector;
		uiSelector = newUiSelector;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, PartsPackage.PAGE__UI_SELECTOR, oldUiSelector, uiSelector));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getSelectionProcessor() {
		return selectionProcessor;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSelectionProcessor(String newSelectionProcessor) {
		String oldSelectionProcessor = selectionProcessor;
		selectionProcessor = newSelectionProcessor;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, PartsPackage.PAGE__SELECTION_PROCESSOR, oldSelectionProcessor, selectionProcessor));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case PartsPackage.PAGE__UI_SELECTOR:
				return getUiSelector();
			case PartsPackage.PAGE__SELECTION_PROCESSOR:
				return getSelectionProcessor();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case PartsPackage.PAGE__UI_SELECTOR:
				setUiSelector((String)newValue);
				return;
			case PartsPackage.PAGE__SELECTION_PROCESSOR:
				setSelectionProcessor((String)newValue);
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
			case PartsPackage.PAGE__UI_SELECTOR:
				setUiSelector(UI_SELECTOR_EDEFAULT);
				return;
			case PartsPackage.PAGE__SELECTION_PROCESSOR:
				setSelectionProcessor(SELECTION_PROCESSOR_EDEFAULT);
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
			case PartsPackage.PAGE__UI_SELECTOR:
				return UI_SELECTOR_EDEFAULT == null ? uiSelector != null : !UI_SELECTOR_EDEFAULT.equals(uiSelector);
			case PartsPackage.PAGE__SELECTION_PROCESSOR:
				return SELECTION_PROCESSOR_EDEFAULT == null ? selectionProcessor != null : !SELECTION_PROCESSOR_EDEFAULT.equals(selectionProcessor);
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
		result.append(" (uiSelector: ");
		result.append(uiSelector);
		result.append(", selectionProcessor: ");
		result.append(selectionProcessor);
		result.append(')');
		return result.toString();
	}

} //PageImpl
