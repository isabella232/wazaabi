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
import org.eclipse.wazaabi.mm.core.widgets.Widget;

import org.eclipse.wazaabi.mm.edp.Context;
import org.eclipse.wazaabi.mm.edp.ContextContent;
import org.eclipse.wazaabi.mm.edp.EdpPackage;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;

import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.State;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Widget</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.WidgetImpl#getAnnotations <em>Annotations</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.WidgetImpl#getContents <em>Contents</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.WidgetImpl#getHandlers <em>Handlers</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.WidgetImpl#getState <em>State</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.impl.WidgetImpl#getStyleRules <em>Style Rules</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class WidgetImpl extends EObjectImpl implements Widget {
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
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected WidgetImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreWidgetsPackage.Literals.WIDGET;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Annotation> getAnnotations() {
		if (annotations == null) {
			annotations = new EObjectContainmentEList<Annotation>(Annotation.class, this, CoreWidgetsPackage.WIDGET__ANNOTATIONS);
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
			contents = new EObjectContainmentEList<ContextContent>(ContextContent.class, this, CoreWidgetsPackage.WIDGET__CONTENTS);
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
			handlers = new EObjectContainmentEList<EventHandler>(EventHandler.class, this, CoreWidgetsPackage.WIDGET__HANDLERS);
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
			eNotify(new ENotificationImpl(this, Notification.SET, CoreWidgetsPackage.WIDGET__STATE, oldState, state));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<StyleRule> getStyleRules() {
		if (styleRules == null) {
			styleRules = new EObjectContainmentEList<StyleRule>(StyleRule.class, this, CoreWidgetsPackage.WIDGET__STYLE_RULES);
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
	public void setAnnotation(String source, String key, String value) {
		if (source != null && !"".equals(source) && key != null //$NON-NLS-1$
				&& !"".equals(key)) { //$NON-NLS-1$
			Annotation annotation = null;
			for (Annotation _annotation : getAnnotations())
				if (source.equals(_annotation.getSource())) {
					annotation = _annotation;
					break;
				}
			if (annotation == null) {
				annotation = org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory.eINSTANCE
						.createAnnotation();
				annotation.setSource(source);
				getAnnotations().add(annotation);
			}
			org.eclipse.wazaabi.mm.core.annotations.AnnotationContent content = null;
			for (org.eclipse.wazaabi.mm.core.annotations.AnnotationContent _content : annotation.getContents())
				if (key.equals(_content.getKey())) {
					content = _content;
					break;
				}
			if (content == null) {
				content = org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory.eINSTANCE
						.createAnnotationContent();
				annotation.getContents().add(content);
			}
			content.setKey(key);
			content.setValue(value);
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CoreWidgetsPackage.WIDGET__ANNOTATIONS:
				return ((InternalEList<?>)getAnnotations()).basicRemove(otherEnd, msgs);
			case CoreWidgetsPackage.WIDGET__CONTENTS:
				return ((InternalEList<?>)getContents()).basicRemove(otherEnd, msgs);
			case CoreWidgetsPackage.WIDGET__HANDLERS:
				return ((InternalEList<?>)getHandlers()).basicRemove(otherEnd, msgs);
			case CoreWidgetsPackage.WIDGET__STYLE_RULES:
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
			case CoreWidgetsPackage.WIDGET__ANNOTATIONS:
				return getAnnotations();
			case CoreWidgetsPackage.WIDGET__CONTENTS:
				return getContents();
			case CoreWidgetsPackage.WIDGET__HANDLERS:
				return getHandlers();
			case CoreWidgetsPackage.WIDGET__STATE:
				return getState();
			case CoreWidgetsPackage.WIDGET__STYLE_RULES:
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
			case CoreWidgetsPackage.WIDGET__ANNOTATIONS:
				getAnnotations().clear();
				getAnnotations().addAll((Collection<? extends Annotation>)newValue);
				return;
			case CoreWidgetsPackage.WIDGET__CONTENTS:
				getContents().clear();
				getContents().addAll((Collection<? extends ContextContent>)newValue);
				return;
			case CoreWidgetsPackage.WIDGET__HANDLERS:
				getHandlers().clear();
				getHandlers().addAll((Collection<? extends EventHandler>)newValue);
				return;
			case CoreWidgetsPackage.WIDGET__STATE:
				setState((State)newValue);
				return;
			case CoreWidgetsPackage.WIDGET__STYLE_RULES:
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
			case CoreWidgetsPackage.WIDGET__ANNOTATIONS:
				getAnnotations().clear();
				return;
			case CoreWidgetsPackage.WIDGET__CONTENTS:
				getContents().clear();
				return;
			case CoreWidgetsPackage.WIDGET__HANDLERS:
				getHandlers().clear();
				return;
			case CoreWidgetsPackage.WIDGET__STATE:
				setState(STATE_EDEFAULT);
				return;
			case CoreWidgetsPackage.WIDGET__STYLE_RULES:
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
			case CoreWidgetsPackage.WIDGET__ANNOTATIONS:
				return annotations != null && !annotations.isEmpty();
			case CoreWidgetsPackage.WIDGET__CONTENTS:
				return contents != null && !contents.isEmpty();
			case CoreWidgetsPackage.WIDGET__HANDLERS:
				return handlers != null && !handlers.isEmpty();
			case CoreWidgetsPackage.WIDGET__STATE:
				return state != STATE_EDEFAULT;
			case CoreWidgetsPackage.WIDGET__STYLE_RULES:
				return styleRules != null && !styleRules.isEmpty();
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
				case CoreWidgetsPackage.WIDGET__CONTENTS: return EdpPackage.CONTEXT__CONTENTS;
				default: return -1;
			}
		}
		if (baseClass == EventDispatcher.class) {
			switch (derivedFeatureID) {
				case CoreWidgetsPackage.WIDGET__HANDLERS: return EdpPackage.EVENT_DISPATCHER__HANDLERS;
				case CoreWidgetsPackage.WIDGET__STATE: return EdpPackage.EVENT_DISPATCHER__STATE;
				default: return -1;
			}
		}
		if (baseClass == StyledElement.class) {
			switch (derivedFeatureID) {
				case CoreWidgetsPackage.WIDGET__STYLE_RULES: return CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES;
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
				case EdpPackage.CONTEXT__CONTENTS: return CoreWidgetsPackage.WIDGET__CONTENTS;
				default: return -1;
			}
		}
		if (baseClass == EventDispatcher.class) {
			switch (baseFeatureID) {
				case EdpPackage.EVENT_DISPATCHER__HANDLERS: return CoreWidgetsPackage.WIDGET__HANDLERS;
				case EdpPackage.EVENT_DISPATCHER__STATE: return CoreWidgetsPackage.WIDGET__STATE;
				default: return -1;
			}
		}
		if (baseClass == StyledElement.class) {
			switch (baseFeatureID) {
				case CoreStylesPackage.STYLED_ELEMENT__STYLE_RULES: return CoreWidgetsPackage.WIDGET__STYLE_RULES;
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
		result.append(')');
		return result.toString();
	}

} //WidgetImpl
