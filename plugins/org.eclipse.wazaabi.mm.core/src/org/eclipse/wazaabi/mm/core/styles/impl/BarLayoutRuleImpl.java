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

import org.eclipse.wazaabi.mm.core.styles.BarLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Bar Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.BarLayoutRuleImpl#isDraggable <em>Draggable</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class BarLayoutRuleImpl extends LayoutRuleImpl implements BarLayoutRule {
	/**
	 * The default value of the '{@link #isDraggable() <em>Draggable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isDraggable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DRAGGABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isDraggable() <em>Draggable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isDraggable()
	 * @generated
	 * @ordered
	 */
	protected boolean draggable = DRAGGABLE_EDEFAULT;

	/**
	 * This is true if the Draggable attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean draggableESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BarLayoutRuleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreStylesPackage.Literals.BAR_LAYOUT_RULE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isDraggable() {
		return draggable;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setDraggable(boolean newDraggable) {
		boolean oldDraggable = draggable;
		draggable = newDraggable;
		boolean oldDraggableESet = draggableESet;
		draggableESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.BAR_LAYOUT_RULE__DRAGGABLE, oldDraggable, draggable, !oldDraggableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetDraggable() {
		boolean oldDraggable = draggable;
		boolean oldDraggableESet = draggableESet;
		draggable = DRAGGABLE_EDEFAULT;
		draggableESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.BAR_LAYOUT_RULE__DRAGGABLE, oldDraggable, DRAGGABLE_EDEFAULT, oldDraggableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetDraggable() {
		return draggableESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreStylesPackage.BAR_LAYOUT_RULE__DRAGGABLE:
				return isDraggable();
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
			case CoreStylesPackage.BAR_LAYOUT_RULE__DRAGGABLE:
				setDraggable((Boolean)newValue);
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
			case CoreStylesPackage.BAR_LAYOUT_RULE__DRAGGABLE:
				unsetDraggable();
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
			case CoreStylesPackage.BAR_LAYOUT_RULE__DRAGGABLE:
				return isSetDraggable();
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
		result.append(" (draggable: ");
		if (draggableESet) result.append(draggable); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //BarLayoutRuleImpl
