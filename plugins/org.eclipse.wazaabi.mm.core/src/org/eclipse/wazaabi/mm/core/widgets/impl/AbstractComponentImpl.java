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

import org.eclipse.wazaabi.mm.core.Direction;

import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Abstract Component</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.AbstractComponentImpl#getId <em>Id</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.AbstractComponentImpl#isFocus <em>Focus</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class AbstractComponentImpl extends WidgetImpl implements AbstractComponent {
	/**
	 * The default value of the '{@link #getId() <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected static final String ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getId() <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected String id = ID_EDEFAULT;

	/**
	 * The default value of the '{@link #isFocus() <em>Focus</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isFocus()
	 * @generated
	 * @ordered
	 */
	protected static final boolean FOCUS_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isFocus() <em>Focus</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isFocus()
	 * @generated
	 * @ordered
	 */
	protected boolean focus = FOCUS_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AbstractComponentImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreWidgetsPackage.Literals.ABSTRACT_COMPONENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getId() {
		return id;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setId(String newId) {
		String oldId = id;
		id = newId;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.ABSTRACT_COMPONENT__ID, oldId, id));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isFocus() {
		return focus;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setFocus(boolean newFocus) {
		boolean oldFocus = focus;
		focus = newFocus;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.ABSTRACT_COMPONENT__FOCUS, oldFocus, focus));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Direction getDirection() {
		org.eclipse.wazaabi.mm.core.styles.DirectionRule rule = (org.eclipse.wazaabi.mm.core.styles.DirectionRule) getFirstStyleRule(
				"direction", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.DIRECTION_RULE);
		if (rule != null)
			return rule.getValue();
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setDirection(Direction direction) {
		org.eclipse.wazaabi.mm.core.styles.DirectionRule rule = (org.eclipse.wazaabi.mm.core.styles.DirectionRule) getFirstStyleRule(
				"direction", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.DIRECTION_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createDirectionRule();
			rule.setPropertyName("direction"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(direction);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getToolTipText() {
		org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
				"tooltip-text", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);
		if (rule != null)
			return rule.getValue();
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setToolTipText(String text) {
		org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
				"tooltip-text", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createStringRule();
			rule.setPropertyName("tooltip-text"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(text);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getErrorText() {
		org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
				"error-text", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);
		if (rule != null)
			return rule.getValue();
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setErrorText(String text) {
		if (text == null || "".equals(text))
			removeFirstStyleRule(
					"error-text", org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE); //$NON-NLS-1$
		else {
			org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
					"error-text", //$NON-NLS-1$
					org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);
			if (rule == null) {
				rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
						.createStringRule();
				rule.setPropertyName("error-text"); //$NON-NLS-1$
				getStyleRules().add(rule);
			}
			rule.setValue(text);
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEnabled(boolean enabled) {
		if (enabled)
			removeFirstStyleRule(
					"enabled", org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.BOOLEAN_RULE); //$NON-NLS-1$
		else {
			org.eclipse.wazaabi.mm.core.styles.BooleanRule rule = (org.eclipse.wazaabi.mm.core.styles.BooleanRule) getFirstStyleRule(
					"enabled", //$NON-NLS-1$
					org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.BOOLEAN_RULE);
			if (rule == null) {
				rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
						.createBooleanRule();
				rule.setPropertyName("enabled"); //$NON-NLS-1$
				getStyleRules().add(rule);
			}
			rule.setValue(false);
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__ID:
				return getId();
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__FOCUS:
				return isFocus();
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
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__ID:
				setId((String)newValue);
				return;
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__FOCUS:
				setFocus((Boolean)newValue);
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
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__ID:
				setId(ID_EDEFAULT);
				return;
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__FOCUS:
				setFocus(FOCUS_EDEFAULT);
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
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__ID:
				return ID_EDEFAULT == null ? id != null : !ID_EDEFAULT.equals(id);
			case CoreWidgetsPackage.ABSTRACT_COMPONENT__FOCUS:
				return focus != FOCUS_EDEFAULT;
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
		result.append(" (id: ");
		result.append(id);
		result.append(", focus: ");
		result.append(focus);
		result.append(')');
		return result.toString();
	}

} //AbstractComponentImpl
