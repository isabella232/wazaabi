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

import org.eclipse.wazaabi.mm.core.Orientation;

import org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Box Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.BoxLayoutRuleImpl#getOrientation <em>Orientation</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.BoxLayoutRuleImpl#getMargin <em>Margin</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.BoxLayoutRuleImpl#getSpacing <em>Spacing</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class BoxLayoutRuleImpl extends LayoutRuleImpl implements BoxLayoutRule {
    /**
     * The default value of the '{@link #getOrientation() <em>Orientation</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getOrientation()
     * @generated
     * @ordered
     */
    protected static final Orientation ORIENTATION_EDEFAULT = Orientation.HORIZONTAL;

    /**
     * The cached value of the '{@link #getOrientation() <em>Orientation</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getOrientation()
     * @generated
     * @ordered
     */
    protected Orientation orientation = ORIENTATION_EDEFAULT;

    /**
     * The default value of the '{@link #getMargin() <em>Margin</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getMargin()
     * @generated
     * @ordered
     */
    protected static final int MARGIN_EDEFAULT = 0;

    /**
     * The cached value of the '{@link #getMargin() <em>Margin</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getMargin()
     * @generated
     * @ordered
     */
    protected int margin = MARGIN_EDEFAULT;

    /**
     * The default value of the '{@link #getSpacing() <em>Spacing</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getSpacing()
     * @generated
     * @ordered
     */
    protected static final int SPACING_EDEFAULT = 0;

    /**
     * The cached value of the '{@link #getSpacing() <em>Spacing</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getSpacing()
     * @generated
     * @ordered
     */
    protected int spacing = SPACING_EDEFAULT;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected BoxLayoutRuleImpl() {
        super();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    protected EClass eStaticClass() {
        return CoreStylesPackage.Literals.BOX_LAYOUT_RULE;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public Orientation getOrientation() {
        return orientation;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setOrientation(Orientation newOrientation) {
        Orientation oldOrientation = orientation;
        orientation = newOrientation == null ? ORIENTATION_EDEFAULT : newOrientation;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.BOX_LAYOUT_RULE__ORIENTATION, oldOrientation, orientation));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public int getMargin() {
        return margin;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setMargin(int newMargin) {
        int oldMargin = margin;
        margin = newMargin;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.BOX_LAYOUT_RULE__MARGIN, oldMargin, margin));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public int getSpacing() {
        return spacing;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setSpacing(int newSpacing) {
        int oldSpacing = spacing;
        spacing = newSpacing;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.BOX_LAYOUT_RULE__SPACING, oldSpacing, spacing));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case CoreStylesPackage.BOX_LAYOUT_RULE__ORIENTATION:
                return getOrientation();
            case CoreStylesPackage.BOX_LAYOUT_RULE__MARGIN:
                return getMargin();
            case CoreStylesPackage.BOX_LAYOUT_RULE__SPACING:
                return getSpacing();
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
            case CoreStylesPackage.BOX_LAYOUT_RULE__ORIENTATION:
                setOrientation((Orientation)newValue);
                return;
            case CoreStylesPackage.BOX_LAYOUT_RULE__MARGIN:
                setMargin((Integer)newValue);
                return;
            case CoreStylesPackage.BOX_LAYOUT_RULE__SPACING:
                setSpacing((Integer)newValue);
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
            case CoreStylesPackage.BOX_LAYOUT_RULE__ORIENTATION:
                setOrientation(ORIENTATION_EDEFAULT);
                return;
            case CoreStylesPackage.BOX_LAYOUT_RULE__MARGIN:
                setMargin(MARGIN_EDEFAULT);
                return;
            case CoreStylesPackage.BOX_LAYOUT_RULE__SPACING:
                setSpacing(SPACING_EDEFAULT);
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
            case CoreStylesPackage.BOX_LAYOUT_RULE__ORIENTATION:
                return orientation != ORIENTATION_EDEFAULT;
            case CoreStylesPackage.BOX_LAYOUT_RULE__MARGIN:
                return margin != MARGIN_EDEFAULT;
            case CoreStylesPackage.BOX_LAYOUT_RULE__SPACING:
                return spacing != SPACING_EDEFAULT;
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
        result.append(" (orientation: ");
        result.append(orientation);
        result.append(", margin: ");
        result.append(margin);
        result.append(", spacing: ");
        result.append(spacing);
        result.append(')');
        return result.toString();
    }

} //BoxLayoutRuleImpl
