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
package org.eclipse.wazaabi.mm.swt.styles.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.Orientation;

import org.eclipse.wazaabi.mm.core.styles.impl.LayoutRuleImpl;

import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Fill Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl#getMarginWidth <em>Margin Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl#getMarginHeight <em>Margin Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl#getSpacing <em>Spacing</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.FillLayoutRuleImpl#getType <em>Type</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class FillLayoutRuleImpl extends LayoutRuleImpl implements FillLayoutRule {
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
	 * The default value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final Orientation TYPE_EDEFAULT = Orientation.HORIZONTAL;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected Orientation type = TYPE_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected FillLayoutRuleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return SWTStylesPackage.Literals.FILL_LAYOUT_RULE;
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
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_WIDTH, oldMarginWidth, marginWidth));
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
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_HEIGHT, oldMarginHeight, marginHeight));
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
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.FILL_LAYOUT_RULE__SPACING, oldSpacing, spacing));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Orientation getType() {
		return type;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setType(Orientation newType) {
		Orientation oldType = type;
		type = newType == null ? TYPE_EDEFAULT : newType;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.FILL_LAYOUT_RULE__TYPE, oldType, type));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_WIDTH:
				return getMarginWidth();
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_HEIGHT:
				return getMarginHeight();
			case SWTStylesPackage.FILL_LAYOUT_RULE__SPACING:
				return getSpacing();
			case SWTStylesPackage.FILL_LAYOUT_RULE__TYPE:
				return getType();
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
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_WIDTH:
				setMarginWidth((Integer)newValue);
				return;
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_HEIGHT:
				setMarginHeight((Integer)newValue);
				return;
			case SWTStylesPackage.FILL_LAYOUT_RULE__SPACING:
				setSpacing((Integer)newValue);
				return;
			case SWTStylesPackage.FILL_LAYOUT_RULE__TYPE:
				setType((Orientation)newValue);
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
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_WIDTH:
				setMarginWidth(MARGIN_WIDTH_EDEFAULT);
				return;
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_HEIGHT:
				setMarginHeight(MARGIN_HEIGHT_EDEFAULT);
				return;
			case SWTStylesPackage.FILL_LAYOUT_RULE__SPACING:
				setSpacing(SPACING_EDEFAULT);
				return;
			case SWTStylesPackage.FILL_LAYOUT_RULE__TYPE:
				setType(TYPE_EDEFAULT);
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
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_WIDTH:
				return marginWidth != MARGIN_WIDTH_EDEFAULT;
			case SWTStylesPackage.FILL_LAYOUT_RULE__MARGIN_HEIGHT:
				return marginHeight != MARGIN_HEIGHT_EDEFAULT;
			case SWTStylesPackage.FILL_LAYOUT_RULE__SPACING:
				return spacing != SPACING_EDEFAULT;
			case SWTStylesPackage.FILL_LAYOUT_RULE__TYPE:
				return type != TYPE_EDEFAULT;
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
		result.append(" (marginWidth: ");
		result.append(marginWidth);
		result.append(", marginHeight: ");
		result.append(marginHeight);
		result.append(", spacing: ");
		result.append(spacing);
		result.append(", type: ");
		result.append(type);
		result.append(')');
		return result.toString();
	}

} //FillLayoutRuleImpl
