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

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Weighted Column Descriptor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.WeightedColumnDescriptorImpl#getWeight <em>Weight</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.WeightedColumnDescriptorImpl#getMinimumWidth <em>Minimum Width</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class WeightedColumnDescriptorImpl extends AbstractColumnDescriptorImpl implements WeightedColumnDescriptor {
	/**
	 * The default value of the '{@link #getWeight() <em>Weight</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getWeight()
	 * @generated
	 * @ordered
	 */
	protected static final int WEIGHT_EDEFAULT = 20;

	/**
	 * The cached value of the '{@link #getWeight() <em>Weight</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getWeight()
	 * @generated
	 * @ordered
	 */
	protected int weight = WEIGHT_EDEFAULT;

	/**
	 * The default value of the '{@link #getMinimumWidth() <em>Minimum Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMinimumWidth()
	 * @generated
	 * @ordered
	 */
	protected static final int MINIMUM_WIDTH_EDEFAULT = 20;

	/**
	 * The cached value of the '{@link #getMinimumWidth() <em>Minimum Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMinimumWidth()
	 * @generated
	 * @ordered
	 */
	protected int minimumWidth = MINIMUM_WIDTH_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected WeightedColumnDescriptorImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreCollectionsStylesPackage.Literals.WEIGHTED_COLUMN_DESCRIPTOR;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getWeight() {
		return weight;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setWeight(int newWeight) {
		int oldWeight = weight;
		weight = newWeight;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__WEIGHT, oldWeight, weight));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getMinimumWidth() {
		return minimumWidth;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMinimumWidth(int newMinimumWidth) {
		int oldMinimumWidth = minimumWidth;
		minimumWidth = newMinimumWidth;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__MINIMUM_WIDTH, oldMinimumWidth, minimumWidth));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__WEIGHT:
				return getWeight();
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				return getMinimumWidth();
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
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__WEIGHT:
				setWeight((Integer)newValue);
				return;
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				setMinimumWidth((Integer)newValue);
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
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__WEIGHT:
				setWeight(WEIGHT_EDEFAULT);
				return;
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				setMinimumWidth(MINIMUM_WIDTH_EDEFAULT);
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
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__WEIGHT:
				return weight != WEIGHT_EDEFAULT;
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				return minimumWidth != MINIMUM_WIDTH_EDEFAULT;
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
		result.append(" (weight: ");
		result.append(weight);
		result.append(", minimumWidth: ");
		result.append(minimumWidth);
		result.append(')');
		return result.toString();
	}

} //WeightedColumnDescriptorImpl
