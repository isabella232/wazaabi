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
package org.eclipse.wazaabi.mm.core.styles.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.Position;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Tabbed Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.TabbedLayoutRuleImpl#isMaximizeVisible <em>Maximize Visible</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.TabbedLayoutRuleImpl#isMinimizeVisible <em>Minimize Visible</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.TabbedLayoutRuleImpl#getPosition <em>Position</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TabbedLayoutRuleImpl extends StackLayoutRuleImpl implements TabbedLayoutRule {
	/**
	 * The default value of the '{@link #isMaximizeVisible() <em>Maximize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isMaximizeVisible()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MAXIMIZE_VISIBLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isMaximizeVisible() <em>Maximize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isMaximizeVisible()
	 * @generated
	 * @ordered
	 */
	protected boolean maximizeVisible = MAXIMIZE_VISIBLE_EDEFAULT;

	/**
	 * This is true if the Maximize Visible attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean maximizeVisibleESet;

	/**
	 * The default value of the '{@link #isMinimizeVisible() <em>Minimize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isMinimizeVisible()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MINIMIZE_VISIBLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isMinimizeVisible() <em>Minimize Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isMinimizeVisible()
	 * @generated
	 * @ordered
	 */
	protected boolean minimizeVisible = MINIMIZE_VISIBLE_EDEFAULT;

	/**
	 * This is true if the Minimize Visible attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean minimizeVisibleESet;

	/**
	 * The default value of the '{@link #getPosition() <em>Position</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPosition()
	 * @generated
	 * @ordered
	 */
	protected static final Position POSITION_EDEFAULT = Position.TOP;

	/**
	 * The cached value of the '{@link #getPosition() <em>Position</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPosition()
	 * @generated
	 * @ordered
	 */
	protected Position position = POSITION_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TabbedLayoutRuleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreStylesPackage.Literals.TABBED_LAYOUT_RULE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isMaximizeVisible() {
		return maximizeVisible;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMaximizeVisible(boolean newMaximizeVisible) {
		boolean oldMaximizeVisible = maximizeVisible;
		maximizeVisible = newMaximizeVisible;
		boolean oldMaximizeVisibleESet = maximizeVisibleESet;
		maximizeVisibleESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE, oldMaximizeVisible, maximizeVisible, !oldMaximizeVisibleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetMaximizeVisible() {
		boolean oldMaximizeVisible = maximizeVisible;
		boolean oldMaximizeVisibleESet = maximizeVisibleESet;
		maximizeVisible = MAXIMIZE_VISIBLE_EDEFAULT;
		maximizeVisibleESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE, oldMaximizeVisible, MAXIMIZE_VISIBLE_EDEFAULT, oldMaximizeVisibleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetMaximizeVisible() {
		return maximizeVisibleESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isMinimizeVisible() {
		return minimizeVisible;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMinimizeVisible(boolean newMinimizeVisible) {
		boolean oldMinimizeVisible = minimizeVisible;
		minimizeVisible = newMinimizeVisible;
		boolean oldMinimizeVisibleESet = minimizeVisibleESet;
		minimizeVisibleESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE, oldMinimizeVisible, minimizeVisible, !oldMinimizeVisibleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetMinimizeVisible() {
		boolean oldMinimizeVisible = minimizeVisible;
		boolean oldMinimizeVisibleESet = minimizeVisibleESet;
		minimizeVisible = MINIMIZE_VISIBLE_EDEFAULT;
		minimizeVisibleESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE, oldMinimizeVisible, MINIMIZE_VISIBLE_EDEFAULT, oldMinimizeVisibleESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetMinimizeVisible() {
		return minimizeVisibleESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Position getPosition() {
		return position;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPosition(Position newPosition) {
		Position oldPosition = position;
		position = newPosition == null ? POSITION_EDEFAULT : newPosition;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.TABBED_LAYOUT_RULE__POSITION, oldPosition, position));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE:
				return isMaximizeVisible();
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE:
				return isMinimizeVisible();
			case CoreStylesPackage.TABBED_LAYOUT_RULE__POSITION:
				return getPosition();
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
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE:
				setMaximizeVisible((Boolean)newValue);
				return;
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE:
				setMinimizeVisible((Boolean)newValue);
				return;
			case CoreStylesPackage.TABBED_LAYOUT_RULE__POSITION:
				setPosition((Position)newValue);
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
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE:
				unsetMaximizeVisible();
				return;
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE:
				unsetMinimizeVisible();
				return;
			case CoreStylesPackage.TABBED_LAYOUT_RULE__POSITION:
				setPosition(POSITION_EDEFAULT);
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
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE:
				return isSetMaximizeVisible();
			case CoreStylesPackage.TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE:
				return isSetMinimizeVisible();
			case CoreStylesPackage.TABBED_LAYOUT_RULE__POSITION:
				return position != POSITION_EDEFAULT;
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
		result.append(" (maximizeVisible: ");
		if (maximizeVisibleESet) result.append(maximizeVisible); else result.append("<unset>");
		result.append(", minimizeVisible: ");
		if (minimizeVisibleESet) result.append(minimizeVisible); else result.append("<unset>");
		result.append(", position: ");
		result.append(position);
		result.append(')');
		return result.toString();
	}

} //TabbedLayoutRuleImpl
