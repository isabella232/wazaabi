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
package org.eclipse.wazaabi.mm.core.widgets.util;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

import org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement;

import org.eclipse.wazaabi.mm.core.styles.StyledElement;

import org.eclipse.wazaabi.mm.core.widgets.*;

import org.eclipse.wazaabi.mm.edp.Context;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage
 * @generated
 */
public class CoreWidgetsSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static CoreWidgetsPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreWidgetsSwitch() {
		if (modelPackage == null) {
			modelPackage = CoreWidgetsPackage.eINSTANCE;
		}
	}

	/**
	 * Checks whether this is a switch for the given package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @parameter ePackage the package in question.
	 * @return whether this is a switch for the given package.
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/**
	 * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the first non-null result returned by a <code>caseXXX</code> call.
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case CoreWidgetsPackage.WIDGET: {
				Widget widget = (Widget)theEObject;
				T result = caseWidget(widget);
				if (result == null) result = caseAnnotatedElement(widget);
				if (result == null) result = caseEventDispatcher(widget);
				if (result == null) result = caseStyledElement(widget);
				if (result == null) result = caseContext(widget);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.PROGRESS_BAR: {
				ProgressBar progressBar = (ProgressBar)theEObject;
				T result = caseProgressBar(progressBar);
				if (result == null) result = caseAbstractComponent(progressBar);
				if (result == null) result = caseWidget(progressBar);
				if (result == null) result = caseAnnotatedElement(progressBar);
				if (result == null) result = caseEventDispatcher(progressBar);
				if (result == null) result = caseStyledElement(progressBar);
				if (result == null) result = caseContext(progressBar);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.CONTAINER: {
				Container container = (Container)theEObject;
				T result = caseContainer(container);
				if (result == null) result = caseAbstractComponent(container);
				if (result == null) result = caseWidget(container);
				if (result == null) result = caseAnnotatedElement(container);
				if (result == null) result = caseEventDispatcher(container);
				if (result == null) result = caseStyledElement(container);
				if (result == null) result = caseContext(container);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.TEXT_COMPONENT: {
				TextComponent textComponent = (TextComponent)theEObject;
				T result = caseTextComponent(textComponent);
				if (result == null) result = caseAbstractComponent(textComponent);
				if (result == null) result = caseWidget(textComponent);
				if (result == null) result = caseAnnotatedElement(textComponent);
				if (result == null) result = caseEventDispatcher(textComponent);
				if (result == null) result = caseStyledElement(textComponent);
				if (result == null) result = caseContext(textComponent);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.ABSTRACT_COMPONENT: {
				AbstractComponent abstractComponent = (AbstractComponent)theEObject;
				T result = caseAbstractComponent(abstractComponent);
				if (result == null) result = caseWidget(abstractComponent);
				if (result == null) result = caseAnnotatedElement(abstractComponent);
				if (result == null) result = caseEventDispatcher(abstractComponent);
				if (result == null) result = caseStyledElement(abstractComponent);
				if (result == null) result = caseContext(abstractComponent);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.ABSTRACT_BUTTON: {
				AbstractButton abstractButton = (AbstractButton)theEObject;
				T result = caseAbstractButton(abstractButton);
				if (result == null) result = caseAbstractComponent(abstractButton);
				if (result == null) result = caseWidget(abstractButton);
				if (result == null) result = caseAnnotatedElement(abstractButton);
				if (result == null) result = caseEventDispatcher(abstractButton);
				if (result == null) result = caseStyledElement(abstractButton);
				if (result == null) result = caseContext(abstractButton);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.PUSH_BUTTON: {
				PushButton pushButton = (PushButton)theEObject;
				T result = casePushButton(pushButton);
				if (result == null) result = caseAbstractButton(pushButton);
				if (result == null) result = caseAbstractComponent(pushButton);
				if (result == null) result = caseWidget(pushButton);
				if (result == null) result = caseAnnotatedElement(pushButton);
				if (result == null) result = caseEventDispatcher(pushButton);
				if (result == null) result = caseStyledElement(pushButton);
				if (result == null) result = caseContext(pushButton);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.LABEL: {
				Label label = (Label)theEObject;
				T result = caseLabel(label);
				if (result == null) result = caseAbstractComponent(label);
				if (result == null) result = caseWidget(label);
				if (result == null) result = caseAnnotatedElement(label);
				if (result == null) result = caseEventDispatcher(label);
				if (result == null) result = caseStyledElement(label);
				if (result == null) result = caseContext(label);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.RADIO_BUTTON: {
				RadioButton radioButton = (RadioButton)theEObject;
				T result = caseRadioButton(radioButton);
				if (result == null) result = caseAbstractButton(radioButton);
				if (result == null) result = caseAbstractComponent(radioButton);
				if (result == null) result = caseWidget(radioButton);
				if (result == null) result = caseAnnotatedElement(radioButton);
				if (result == null) result = caseEventDispatcher(radioButton);
				if (result == null) result = caseStyledElement(radioButton);
				if (result == null) result = caseContext(radioButton);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.CHECK_BOX: {
				CheckBox checkBox = (CheckBox)theEObject;
				T result = caseCheckBox(checkBox);
				if (result == null) result = caseAbstractButton(checkBox);
				if (result == null) result = caseAbstractComponent(checkBox);
				if (result == null) result = caseWidget(checkBox);
				if (result == null) result = caseAnnotatedElement(checkBox);
				if (result == null) result = caseEventDispatcher(checkBox);
				if (result == null) result = caseStyledElement(checkBox);
				if (result == null) result = caseContext(checkBox);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.SLIDER: {
				Slider slider = (Slider)theEObject;
				T result = caseSlider(slider);
				if (result == null) result = caseAbstractComponent(slider);
				if (result == null) result = caseWidget(slider);
				if (result == null) result = caseAnnotatedElement(slider);
				if (result == null) result = caseEventDispatcher(slider);
				if (result == null) result = caseStyledElement(slider);
				if (result == null) result = caseContext(slider);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.SPINNER: {
				Spinner spinner = (Spinner)theEObject;
				T result = caseSpinner(spinner);
				if (result == null) result = caseAbstractComponent(spinner);
				if (result == null) result = caseWidget(spinner);
				if (result == null) result = caseAnnotatedElement(spinner);
				if (result == null) result = caseEventDispatcher(spinner);
				if (result == null) result = caseStyledElement(spinner);
				if (result == null) result = caseContext(spinner);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.SCALE: {
				Scale scale = (Scale)theEObject;
				T result = caseScale(scale);
				if (result == null) result = caseAbstractComponent(scale);
				if (result == null) result = caseWidget(scale);
				if (result == null) result = caseAnnotatedElement(scale);
				if (result == null) result = caseEventDispatcher(scale);
				if (result == null) result = caseStyledElement(scale);
				if (result == null) result = caseContext(scale);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.COLLECTION: {
				Collection collection = (Collection)theEObject;
				T result = caseCollection(collection);
				if (result == null) result = caseAbstractComponent(collection);
				if (result == null) result = caseWidget(collection);
				if (result == null) result = caseAnnotatedElement(collection);
				if (result == null) result = caseEventDispatcher(collection);
				if (result == null) result = caseStyledElement(collection);
				if (result == null) result = caseContext(collection);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreWidgetsPackage.MENU_COMPONENT: {
				MenuComponent menuComponent = (MenuComponent)theEObject;
				T result = caseMenuComponent(menuComponent);
				if (result == null) result = caseWidget(menuComponent);
				if (result == null) result = caseAnnotatedElement(menuComponent);
				if (result == null) result = caseEventDispatcher(menuComponent);
				if (result == null) result = caseStyledElement(menuComponent);
				if (result == null) result = caseContext(menuComponent);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Widget</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Widget</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseWidget(Widget object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Progress Bar</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Progress Bar</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseProgressBar(ProgressBar object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Container</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Container</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseContainer(Container object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Text Component</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Text Component</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseTextComponent(TextComponent object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Abstract Component</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Abstract Component</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAbstractComponent(AbstractComponent object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Abstract Button</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Abstract Button</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAbstractButton(AbstractButton object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Push Button</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Push Button</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casePushButton(PushButton object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Label</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Label</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLabel(Label object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Radio Button</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Radio Button</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseRadioButton(RadioButton object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Check Box</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Check Box</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseCheckBox(CheckBox object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Slider</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Slider</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSlider(Slider object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Spinner</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Spinner</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSpinner(Spinner object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Scale</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Scale</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseScale(Scale object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Collection</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Collection</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseCollection(Collection object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Menu Component</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Menu Component</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseMenuComponent(MenuComponent object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotated Element</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotated Element</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAnnotatedElement(AnnotatedElement object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Context</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Context</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseContext(Context object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Event Dispatcher</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Event Dispatcher</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseEventDispatcher(EventDispatcher object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Styled Element</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Styled Element</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseStyledElement(StyledElement object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch, but this is the last case anyway.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

} //CoreWidgetsSwitch
