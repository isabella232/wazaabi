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

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.Orientation;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Slider;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Slider</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.SliderImpl#getValue <em>Value</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SliderImpl extends AbstractComponentImpl implements Slider {
	/**
	 * The default value of the '{@link #getValue() <em>Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getValue()
	 * @generated
	 * @ordered
	 */
	protected static final int VALUE_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getValue() <em>Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getValue()
	 * @generated
	 * @ordered
	 */
	protected int value = VALUE_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SliderImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreWidgetsPackage.Literals.SLIDER;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getValue() {
		return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setValue(int newValue) {
		int oldValue = value;
		value = newValue;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.SLIDER__VALUE, oldValue, value));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Orientation getOrientation() {
		org.eclipse.wazaabi.mm.core.styles.OrientationRule rule = (org.eclipse.wazaabi.mm.core.styles.OrientationRule) getFirstStyleRule(
				"orientation", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.ORIENTATION_RULE);
		if (rule != null)
			return rule.getValue();
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setOrientation(Orientation orientation) {
		org.eclipse.wazaabi.mm.core.styles.OrientationRule rule = (org.eclipse.wazaabi.mm.core.styles.OrientationRule) getFirstStyleRule(
				"orientation", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.ORIENTATION_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createOrientationRule();
			rule.setPropertyName("orientation"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(orientation);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMaximum(int maximum) {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"maximum", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createIntRule();
			rule.setPropertyName("maximum"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(maximum);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getMaximum() {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"maximum", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule != null)
			return rule.getValue();
		return 100;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMinimum(int minimum) {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"minimum", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createIntRule();
			rule.setPropertyName("minimum"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(minimum);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getMinimum() {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"minimum", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule != null)
			return rule.getValue();
		return 0;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getPageIncrement() {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"pageIncrement", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule != null)
			return rule.getValue();
		return 10;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPageIncrement(int pageIncrement) {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"pageIncrement", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createIntRule();
			rule.setPropertyName("pageIncrement"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(pageIncrement);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getIncrement() {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"pageIncrement", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule != null)
			return rule.getValue();
		return 10;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIncrement(int increment) {
		org.eclipse.wazaabi.mm.core.styles.IntRule rule = (org.eclipse.wazaabi.mm.core.styles.IntRule) getFirstStyleRule(
				"increment", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.INT_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createIntRule();
			rule.setPropertyName("increment"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(increment);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreWidgetsPackage.SLIDER__VALUE:
				return getValue();
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
			case CoreWidgetsPackage.SLIDER__VALUE:
				setValue((Integer)newValue);
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
			case CoreWidgetsPackage.SLIDER__VALUE:
				setValue(VALUE_EDEFAULT);
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
			case CoreWidgetsPackage.SLIDER__VALUE:
				return value != VALUE_EDEFAULT;
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
		result.append(" (value: ");
		result.append(value);
		result.append(')');
		return result.toString();
	}

} //SliderImpl
