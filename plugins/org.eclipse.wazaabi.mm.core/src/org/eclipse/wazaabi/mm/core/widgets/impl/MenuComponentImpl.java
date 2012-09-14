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
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.wazaabi.mm.core.annotations.Annotation;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;

import org.eclipse.wazaabi.mm.edp.Context;
import org.eclipse.wazaabi.mm.edp.ContextContent;
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;

import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.State;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Menu Component</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getAnnotations <em>Annotations</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getContents <em>Contents</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getHandlers <em>Handlers</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getState <em>State</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getStyleRules <em>Style Rules</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getChildren <em>Children</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#getText <em>Text</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.MenuComponentImpl#isEnabled <em>Enabled</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class MenuComponentImpl extends EObjectImpl implements MenuComponent {
	/**
	 * The cached value of the '{@link #getAnnotations() <em>Annotations</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAnnotations()
	 * @generated
	 * @ordered
	 */
	protected EList<Annotation> annotations;

	/**
	 * The cached value of the '{@link #getContents() <em>Contents</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContents()
	 * @generated
	 * @ordered
	 */
	protected EList<ContextContent> contents;

	/**
	 * The cached value of the '{@link #getHandlers() <em>Handlers</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHandlers()
	 * @generated
	 * @ordered
	 */
	protected EList<EventHandler> handlers;

	/**
	 * The default value of the '{@link #getState() <em>State</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getState()
	 * @generated
	 * @ordered
	 */
	protected static final State STATE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getState() <em>State</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getState()
	 * @generated
	 * @ordered
	 */
	protected State state = STATE_EDEFAULT;

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
	public EList<Annotation> getAnnotations() {
		if (annotations == null) {
			annotations = new EObjectContainmentEList<Annotation>(Annotation.class, this, CoreWidgetsPackage.MENU_COMPONENT__ANNOTATIONS);
		}
		return annotations;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ContextContent> getContents() {
		if (contents == null) {
			contents = new EObjectContainmentEList<ContextContent>(ContextContent.class, this, CoreWidgetsPackage.MENU_COMPONENT__CONTENTS);
		}
		return contents;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<EventHandler> getHandlers() {
		if (handlers == null) {
			handlers = new EObjectContainmentEList<EventHandler>(EventHandler.class, this, CoreWidgetsPackage.MENU_COMPONENT__HANDLERS);
		}
		return handlers;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public State getState() {
		return state;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setState(State newState) {
		State oldState = state;
		state = newState == null ? STATE_EDEFAULT : newState;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.MENU_COMPONENT__STATE, oldState, state));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<StyleRule> getStyleRules() {
		if (styleRules == null) {
			styleRules = new EObjectContainmentEList<StyleRule>(StyleRule.class, this, CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES);
		}
		return styleRules;
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
	public boolean containsKey(String key) {
		return containsKey(key, false);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean containsKey(String key, boolean local) {
		if (key == null || "".equals(key)) //$NON-NLS-1$
			return false;
		for (int i = 0; i < getContents().size(); i++) {
			ContextContent content = (ContextContent) getContents().get(i);
			if (key.equals(content.getKey()))
				return true;
		}
		if (!local
				&& eContainer() instanceof org.eclipse.wazaabi.mm.edp.Context)
			return ((org.eclipse.wazaabi.mm.edp.Context) eContainer())
					.containsKey(key, false);
		return false;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object get(String key) {
		if (key == null || "".equals(key)) //$NON-NLS-1$
			return null;
		for (int i = 0; i < getContents().size(); i++) {
			ContextContent content = (ContextContent) getContents().get(i);
			if (key.equals(content.getKey()))
				return content.getValue();
		}
		if (eContainer() instanceof org.eclipse.wazaabi.mm.edp.Context)
			return ((org.eclipse.wazaabi.mm.edp.Context) eContainer())
					.get(key);
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void remove(String key) {
		if (key == null || "".equals(key)) //$NON-NLS-1$
			return;
		
		ContextContent content = null;
		boolean found = false;
		for (int i = 0; i < getContents().size(); i++) {
			content = (ContextContent) getContents().get(i);
			if (key.equals(content.getKey())) {
				found = true;
				break;
			}
		}
		if (found)
			getContents().remove(content);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void set(String key, Object value) {
		if (key == null || "".equals(key)) //$NON-NLS-1$
			return;
		
		for (int i = 0; i < getContents().size(); i++) {
			ContextContent content = (ContextContent) getContents().get(i);
			if (key.equals(content.getKey())) {
				content.setValue(value);
				return;
			}
		}
		ContextContent newContent = org.eclipse.wazaabi.mm.edp.EdpFactory.eINSTANCE
				.createContextContent();
		newContent.setKey(key);
		newContent.setValue(value);
		getContents().add(newContent);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Annotation> getAnnotations(String source) {
		// TODO: implement this method
		// Ensure that you remove @generated or mark it @generated NOT
		throw new UnsupportedOperationException();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getValue(String source, String key) {
		// TODO: implement this method
		// Ensure that you remove @generated or mark it @generated NOT
		throw new UnsupportedOperationException();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String setValue(String source, String key, String value) {
		// TODO: implement this method
		// Ensure that you remove @generated or mark it @generated NOT
		throw new UnsupportedOperationException();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CoreWidgetsPackage.MENU_COMPONENT__ANNOTATIONS:
				return ((InternalEList<?>)getAnnotations()).basicRemove(otherEnd, msgs);
			case CoreWidgetsPackage.MENU_COMPONENT__CONTENTS:
				return ((InternalEList<?>)getContents()).basicRemove(otherEnd, msgs);
			case CoreWidgetsPackage.MENU_COMPONENT__HANDLERS:
				return ((InternalEList<?>)getHandlers()).basicRemove(otherEnd, msgs);
			case CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES:
				return ((InternalEList<?>)getStyleRules()).basicRemove(otherEnd, msgs);
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
			case CoreWidgetsPackage.MENU_COMPONENT__ANNOTATIONS:
				return getAnnotations();
			case CoreWidgetsPackage.MENU_COMPONENT__CONTENTS:
				return getContents();
			case CoreWidgetsPackage.MENU_COMPONENT__HANDLERS:
				return getHandlers();
			case CoreWidgetsPackage.MENU_COMPONENT__STATE:
				return getState();
			case CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES:
				return getStyleRules();
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
			case CoreWidgetsPackage.MENU_COMPONENT__ANNOTATIONS:
				getAnnotations().clear();
				getAnnotations().addAll((Collection<? extends Annotation>)newValue);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__CONTENTS:
				getContents().clear();
				getContents().addAll((Collection<? extends ContextContent>)newValue);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__HANDLERS:
				getHandlers().clear();
				getHandlers().addAll((Collection<? extends EventHandler>)newValue);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__STATE:
				setState((State)newValue);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES:
				getStyleRules().clear();
				getStyleRules().addAll((Collection<? extends StyleRule>)newValue);
				return;
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
			case CoreWidgetsPackage.MENU_COMPONENT__ANNOTATIONS:
				getAnnotations().clear();
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__CONTENTS:
				getContents().clear();
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__HANDLERS:
				getHandlers().clear();
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__STATE:
				setState(STATE_EDEFAULT);
				return;
			case CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES:
				getStyleRules().clear();
				return;
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
			case CoreWidgetsPackage.MENU_COMPONENT__ANNOTATIONS:
				return annotations != null && !annotations.isEmpty();
			case CoreWidgetsPackage.MENU_COMPONENT__CONTENTS:
				return contents != null && !contents.isEmpty();
			case CoreWidgetsPackage.MENU_COMPONENT__HANDLERS:
				return handlers != null && !handlers.isEmpty();
			case CoreWidgetsPackage.MENU_COMPONENT__STATE:
				return state != STATE_EDEFAULT;
			case CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES:
				return styleRules != null && !styleRules.isEmpty();
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
	public int eBaseStructuralFeatureID(int derivedFeatureID, Class<?> baseClass) {
		if (baseClass == Context.class) {
			switch (derivedFeatureID) {
				case CoreWidgetsPackage.MENU_COMPONENT__CONTENTS: return EdpPackage.CONTEXT__CONTENTS;
				default: return -1;
			}
		}
		if (baseClass == EventDispatcher.class) {
			switch (derivedFeatureID) {
				case CoreWidgetsPackage.MENU_COMPONENT__HANDLERS: return EdpPackage.EVENT_DISPATCHER__HANDLERS;
				case CoreWidgetsPackage.MENU_COMPONENT__STATE: return EdpPackage.EVENT_DISPATCHER__STATE;
				default: return -1;
			}
		}
		if (baseClass == StyledElement.class) {
			switch (derivedFeatureID) {
				case CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES: return CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES;
				default: return -1;
			}
		}
		return super.eBaseStructuralFeatureID(derivedFeatureID, baseClass);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int eDerivedStructuralFeatureID(int baseFeatureID, Class<?> baseClass) {
		if (baseClass == Context.class) {
			switch (baseFeatureID) {
				case EdpPackage.CONTEXT__CONTENTS: return CoreWidgetsPackage.MENU_COMPONENT__CONTENTS;
				default: return -1;
			}
		}
		if (baseClass == EventDispatcher.class) {
			switch (baseFeatureID) {
				case EdpPackage.EVENT_DISPATCHER__HANDLERS: return CoreWidgetsPackage.MENU_COMPONENT__HANDLERS;
				case EdpPackage.EVENT_DISPATCHER__STATE: return CoreWidgetsPackage.MENU_COMPONENT__STATE;
				default: return -1;
			}
		}
		if (baseClass == StyledElement.class) {
			switch (baseFeatureID) {
				case CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES: return CoreWidgetsPackage.MENU_COMPONENT__STYLE_RULES;
				default: return -1;
			}
		}
		return super.eDerivedStructuralFeatureID(baseFeatureID, baseClass);
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
		result.append(" (state: ");
		result.append(state);
		result.append(", text: ");
		result.append(text);
		result.append(", enabled: ");
		result.append(enabled);
		result.append(')');
		return result.toString();
	}

} //MenuComponentImpl
