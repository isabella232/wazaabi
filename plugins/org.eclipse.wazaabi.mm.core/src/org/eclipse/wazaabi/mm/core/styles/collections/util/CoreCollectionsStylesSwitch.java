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
package org.eclipse.wazaabi.mm.core.styles.collections.util;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;

import org.eclipse.wazaabi.mm.core.styles.collections.*;

import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;

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
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage
 * @generated
 */
public class CoreCollectionsStylesSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static CoreCollectionsStylesPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreCollectionsStylesSwitch() {
		if (modelPackage == null) {
			modelPackage = CoreCollectionsStylesPackage.eINSTANCE;
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
			case CoreCollectionsStylesPackage.LOOK_AND_FEEL_RULE: {
				LookAndFeelRule lookAndFeelRule = (LookAndFeelRule)theEObject;
				T result = caseLookAndFeelRule(lookAndFeelRule);
				if (result == null) result = caseStyleRule(lookAndFeelRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR: {
				AbstractColumnDescriptor abstractColumnDescriptor = (AbstractColumnDescriptor)theEObject;
				T result = caseAbstractColumnDescriptor(abstractColumnDescriptor);
				if (result == null) result = caseStyleRule(abstractColumnDescriptor);
				if (result == null) result = caseParameterized(abstractColumnDescriptor);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreCollectionsStylesPackage.PATH_SELECTOR: {
				PathSelector pathSelector = (PathSelector)theEObject;
				T result = casePathSelector(pathSelector);
				if (result == null) result = caseStyleRule(pathSelector);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreCollectionsStylesPackage.DYNAMIC_PROVIDER: {
				DynamicProvider dynamicProvider = (DynamicProvider)theEObject;
				T result = caseDynamicProvider(dynamicProvider);
				if (result == null) result = caseStyleRule(dynamicProvider);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR: {
				ColumnDescriptor columnDescriptor = (ColumnDescriptor)theEObject;
				T result = caseColumnDescriptor(columnDescriptor);
				if (result == null) result = caseAbstractColumnDescriptor(columnDescriptor);
				if (result == null) result = caseStyleRule(columnDescriptor);
				if (result == null) result = caseParameterized(columnDescriptor);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR: {
				WeightedColumnDescriptor weightedColumnDescriptor = (WeightedColumnDescriptor)theEObject;
				T result = caseWeightedColumnDescriptor(weightedColumnDescriptor);
				if (result == null) result = caseAbstractColumnDescriptor(weightedColumnDescriptor);
				if (result == null) result = caseStyleRule(weightedColumnDescriptor);
				if (result == null) result = caseParameterized(weightedColumnDescriptor);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Look And Feel Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Look And Feel Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLookAndFeelRule(LookAndFeelRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Abstract Column Descriptor</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Abstract Column Descriptor</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAbstractColumnDescriptor(AbstractColumnDescriptor object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Path Selector</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Path Selector</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T casePathSelector(PathSelector object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Dynamic Provider</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Dynamic Provider</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDynamicProvider(DynamicProvider object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Column Descriptor</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Column Descriptor</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseColumnDescriptor(ColumnDescriptor object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Weighted Column Descriptor</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Weighted Column Descriptor</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseWeightedColumnDescriptor(WeightedColumnDescriptor object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Style Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Style Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseStyleRule(StyleRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Parameterized</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Parameterized</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseParameterized(Parameterized object) {
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

} //CoreCollectionsStylesSwitch
