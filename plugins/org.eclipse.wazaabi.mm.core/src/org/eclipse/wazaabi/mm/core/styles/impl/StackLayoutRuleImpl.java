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

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Stack Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.StackLayoutRuleImpl#getMarginHeight <em>Margin Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.StackLayoutRuleImpl#getMarginWidth <em>Margin Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.StackLayoutRuleImpl#getTop <em>Top</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class StackLayoutRuleImpl extends LayoutRuleImpl implements StackLayoutRule {
	/**
     * The default value of the '{@link #getMarginHeight() <em>Margin Height</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getMarginHeight()
     * @generated
     * @ordered
     */
	protected static final int MARGIN_HEIGHT_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getMarginHeight() <em>Margin Height</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getMarginHeight()
     * @generated
     * @ordered
     */
	protected int marginHeight = MARGIN_HEIGHT_EDEFAULT;

	/**
     * The default value of the '{@link #getMarginWidth() <em>Margin Width</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getMarginWidth()
     * @generated
     * @ordered
     */
	protected static final int MARGIN_WIDTH_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getMarginWidth() <em>Margin Width</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getMarginWidth()
     * @generated
     * @ordered
     */
	protected int marginWidth = MARGIN_WIDTH_EDEFAULT;

	/**
     * The default value of the '{@link #getTop() <em>Top</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getTop()
     * @generated
     * @ordered
     */
	protected static final int TOP_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getTop() <em>Top</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getTop()
     * @generated
     * @ordered
     */
	protected int top = TOP_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected StackLayoutRuleImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreStylesPackage.Literals.STACK_LAYOUT_RULE;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getMarginHeight() {
        return marginHeight;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setMarginHeight(int newMarginHeight) {
        int oldMarginHeight = marginHeight;
        marginHeight = newMarginHeight;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_HEIGHT, oldMarginHeight, marginHeight));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getMarginWidth() {
        return marginWidth;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setMarginWidth(int newMarginWidth) {
        int oldMarginWidth = marginWidth;
        marginWidth = newMarginWidth;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_WIDTH, oldMarginWidth, marginWidth));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getTop() {
        return top;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setTop(int newTop) {
        int oldTop = top;
        top = newTop;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.STACK_LAYOUT_RULE__TOP, oldTop, top));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_HEIGHT:
                return getMarginHeight();
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_WIDTH:
                return getMarginWidth();
            case CoreStylesPackage.STACK_LAYOUT_RULE__TOP:
                return getTop();
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
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_HEIGHT:
                setMarginHeight((Integer)newValue);
                return;
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_WIDTH:
                setMarginWidth((Integer)newValue);
                return;
            case CoreStylesPackage.STACK_LAYOUT_RULE__TOP:
                setTop((Integer)newValue);
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
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_HEIGHT:
                setMarginHeight(MARGIN_HEIGHT_EDEFAULT);
                return;
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_WIDTH:
                setMarginWidth(MARGIN_WIDTH_EDEFAULT);
                return;
            case CoreStylesPackage.STACK_LAYOUT_RULE__TOP:
                setTop(TOP_EDEFAULT);
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
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_HEIGHT:
                return marginHeight != MARGIN_HEIGHT_EDEFAULT;
            case CoreStylesPackage.STACK_LAYOUT_RULE__MARGIN_WIDTH:
                return marginWidth != MARGIN_WIDTH_EDEFAULT;
            case CoreStylesPackage.STACK_LAYOUT_RULE__TOP:
                return top != TOP_EDEFAULT;
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
        result.append(" (marginHeight: ");
        result.append(marginHeight);
        result.append(", marginWidth: ");
        result.append(marginWidth);
        result.append(", top: ");
        result.append(top);
        result.append(')');
        return result.toString();
    }

} //StackLayoutRuleImpl
