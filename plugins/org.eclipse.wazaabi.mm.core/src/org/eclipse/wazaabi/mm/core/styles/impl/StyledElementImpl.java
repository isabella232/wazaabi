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

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Styled Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.StyledElementImpl#getStyleRules <em>Style Rules</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class StyledElementImpl extends EObjectImpl implements StyledElement {
	/**
	 * The cached value of the '{@link #getStyleRules() <em>Style Rules</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStyleRules()
	 * @generated
	 * @ordered
	 */
	protected EList<StyleRule> styleRules;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected StyledElementImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreStylesPackage.Literals.STYLED_ELEMENT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<StyleRule> getStyleRules() {
		if (styleRules == null) {
			styleRules = new EObjectContainmentEList<StyleRule>(StyleRule.class, this, CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES);
		}
		return styleRules;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public StyleRule getFirstStyleRule(String propertyName, EClass eClass) {
		if (propertyName == null || "".equals(propertyName)) //$NON-NLS-1$
			return null;
		for (StyleRule rule : getStyleRules())
			if (propertyName.equals(rule.getPropertyName())
					&& (eClass == null || (eClass != null && eClass == rule
							.eClass())))
				return rule;
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void removeFirstStyleRule(String propertyName, EClass eClass) {
		if (propertyName == null || "".equals(propertyName)) //$NON-NLS-1$
			return;
		StyleRule toRemove = null;
		for (StyleRule rule : getStyleRules())
			if (propertyName.equals(rule.getPropertyName())
					&& ((eClass != null && eClass == rule.eClass()) || eClass == null)) {
				toRemove = rule;
				break;
			}
		if (toRemove != null)
			getStyleRules().remove(toRemove);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES:
				return ((InternalEList<?>)getStyleRules()).basicRemove(otherEnd, msgs);
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
			case CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES:
				return getStyleRules();
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
			case CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES:
				getStyleRules().clear();
				getStyleRules().addAll((Collection<? extends StyleRule>)newValue);
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
			case CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES:
				getStyleRules().clear();
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
			case CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES:
				return styleRules != null && !styleRules.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //StyledElementImpl
