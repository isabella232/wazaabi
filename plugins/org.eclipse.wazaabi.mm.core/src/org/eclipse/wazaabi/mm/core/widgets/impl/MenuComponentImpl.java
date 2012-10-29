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

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Menu Component</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getChildren <em>Children</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getText <em>Text</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#isEnabled <em>Enabled</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class MenuComponentImpl extends WidgetImpl implements MenuComponent {
	/**
	 * The cached value of the '{@link #getChildren() <em>Children</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getChildren()
	 * @generated
	 * @ordered
	 */
	protected EList<MenuComponent> children;

	/**
	 * The default value of the '{@link #getText() <em>Text</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getText()
	 * @generated
	 * @ordered
	 */
	protected static final String TEXT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getText() <em>Text</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getText()
	 * @generated
	 * @ordered
	 */
	protected String text = TEXT_EDEFAULT;

	/**
	 * The default value of the '{@link #isEnabled() <em>Enabled</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isEnabled()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ENABLED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isEnabled() <em>Enabled</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isEnabled()
	 * @generated
	 * @ordered
	 */
	protected boolean enabled = ENABLED_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected MenuComponentImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreWidgetsPackage.Literals.MENU_COMPONENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<MenuComponent> getChildren() {
		if (children == null) {
			children = new EObjectContainmentEList<MenuComponent>(MenuComponent.class, this, CoreWidgetsPackage.MENU_COMPONENT__CHILDREN);
		}
		return children;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getText() {
		return text;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setText(String newText) {
		String oldText = text;
		text = newText;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.MENU_COMPONENT__TEXT, oldText, text));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isEnabled() {
		return enabled;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEnabled(boolean newEnabled) {
		boolean oldEnabled = enabled;
		enabled = newEnabled;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.MENU_COMPONENT__ENABLED, oldEnabled, enabled));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setType(String type) {
		org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
						"type", //$NON-NLS-1$
						org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);
				if (rule == null) {
					rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
							.createStringRule();
					rule.setPropertyName("type"); //$NON-NLS-1$
					getStyleRules().add(rule);
				}
				rule.setValue(text);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getType() {
		org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
					"type", //$NON-NLS-1$
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
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CoreWidgetsPackage.MENU_COMPONENT__CHILDREN:
				return ((InternalEList<?>)getChildren()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreWidgetsPackage.MENU_COMPONENT__CHILDREN:
				return getChildren();
			case CoreWidgetsPackage.MENU_COMPONENT__TEXT:
				return getText();
			case CoreWidgetsPackage.MENU_COMPONENT__ENABLED:
				return isEnabled();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case CoreWidgetsPackage.MENU_COMPONENT__CHILDREN:
				getChildren().clear();
				getChildren().addAll((Collection<? extends MenuComponent>)newValue);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__TEXT:
				setText((String)newValue);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__ENABLED:
				setEnabled((Boolean)newValue);
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
			case CoreWidgetsPackage.MENU_COMPONENT__CHILDREN:
				getChildren().clear();
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__TEXT:
				setText(TEXT_EDEFAULT);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__ENABLED:
				setEnabled(ENABLED_EDEFAULT);
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
			case CoreWidgetsPackage.MENU_COMPONENT__CHILDREN:
				return children != null && !children.isEmpty();
			case CoreWidgetsPackage.MENU_COMPONENT__TEXT:
				return TEXT_EDEFAULT == null ? text != null : !TEXT_EDEFAULT.equals(text);
			case CoreWidgetsPackage.MENU_COMPONENT__ENABLED:
				return enabled != ENABLED_EDEFAULT;
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
		result.append(" (text: ");
		result.append(text);
		result.append(", enabled: ");
		result.append(enabled);
		result.append(')');
		return result.toString();
	}

} //MenuComponentImpl
