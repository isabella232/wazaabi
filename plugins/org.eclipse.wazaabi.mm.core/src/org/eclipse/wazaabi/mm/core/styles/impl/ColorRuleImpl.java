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
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Color Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl#getRed <em>Red</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl#getGreen <em>Green</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.ColorRuleImpl#getBlue <em>Blue</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ColorRuleImpl extends EObjectImpl implements ColorRule {
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
     * The default value of the '{@link #getRed() <em>Red</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getRed()
     * @generated
     * @ordered
     */
	protected static final int RED_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getRed() <em>Red</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getRed()
     * @generated
     * @ordered
     */
	protected int red = RED_EDEFAULT;

	/**
     * The default value of the '{@link #getGreen() <em>Green</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getGreen()
     * @generated
     * @ordered
     */
	protected static final int GREEN_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getGreen() <em>Green</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getGreen()
     * @generated
     * @ordered
     */
	protected int green = GREEN_EDEFAULT;

	/**
     * The default value of the '{@link #getBlue() <em>Blue</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getBlue()
     * @generated
     * @ordered
     */
	protected static final int BLUE_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getBlue() <em>Blue</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getBlue()
     * @generated
     * @ordered
     */
	protected int blue = BLUE_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ColorRuleImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreStylesPackage.Literals.COLOR_RULE;
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
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.COLOR_RULE__PROPERTY_NAME, oldPropertyName, propertyName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getRed() {
        return red;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setRed(int newRed) {
        int oldRed = red;
        red = newRed;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.COLOR_RULE__RED, oldRed, red));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getGreen() {
        return green;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setGreen(int newGreen) {
        int oldGreen = green;
        green = newGreen;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.COLOR_RULE__GREEN, oldGreen, green));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getBlue() {
        return blue;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setBlue(int newBlue) {
        int oldBlue = blue;
        blue = newBlue;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.COLOR_RULE__BLUE, oldBlue, blue));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case CoreStylesPackage.COLOR_RULE__PROPERTY_NAME:
                return getPropertyName();
            case CoreStylesPackage.COLOR_RULE__RED:
                return getRed();
            case CoreStylesPackage.COLOR_RULE__GREEN:
                return getGreen();
            case CoreStylesPackage.COLOR_RULE__BLUE:
                return getBlue();
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
            case CoreStylesPackage.COLOR_RULE__PROPERTY_NAME:
                setPropertyName((String)newValue);
                return;
            case CoreStylesPackage.COLOR_RULE__RED:
                setRed((Integer)newValue);
                return;
            case CoreStylesPackage.COLOR_RULE__GREEN:
                setGreen((Integer)newValue);
                return;
            case CoreStylesPackage.COLOR_RULE__BLUE:
                setBlue((Integer)newValue);
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
            case CoreStylesPackage.COLOR_RULE__PROPERTY_NAME:
                setPropertyName(PROPERTY_NAME_EDEFAULT);
                return;
            case CoreStylesPackage.COLOR_RULE__RED:
                setRed(RED_EDEFAULT);
                return;
            case CoreStylesPackage.COLOR_RULE__GREEN:
                setGreen(GREEN_EDEFAULT);
                return;
            case CoreStylesPackage.COLOR_RULE__BLUE:
                setBlue(BLUE_EDEFAULT);
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
            case CoreStylesPackage.COLOR_RULE__PROPERTY_NAME:
                return PROPERTY_NAME_EDEFAULT == null ? propertyName != null : !PROPERTY_NAME_EDEFAULT.equals(propertyName);
            case CoreStylesPackage.COLOR_RULE__RED:
                return red != RED_EDEFAULT;
            case CoreStylesPackage.COLOR_RULE__GREEN:
                return green != GREEN_EDEFAULT;
            case CoreStylesPackage.COLOR_RULE__BLUE:
                return blue != BLUE_EDEFAULT;
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
        result.append(", red: ");
        result.append(red);
        result.append(", green: ");
        result.append(green);
        result.append(", blue: ");
        result.append(blue);
        result.append(')');
        return result.toString();
    }

} //ColorRuleImpl
