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

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;

import org.eclipse.wazaabi.mm.core.styles.collections.*;

import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage
 * @generated
 */
public class CoreCollectionsStylesAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static CoreCollectionsStylesPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreCollectionsStylesAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = CoreCollectionsStylesPackage.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
	 * <!-- end-user-doc -->
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage) {
			return true;
		}
		if (object instanceof EObject) {
			return ((EObject)object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected CoreCollectionsStylesSwitch<Adapter> modelSwitch =
		new CoreCollectionsStylesSwitch<Adapter>() {
			@Override
			public Adapter caseLookAndFeelRule(LookAndFeelRule object) {
				return createLookAndFeelRuleAdapter();
			}
			@Override
			public Adapter caseAbstractColumnDescriptor(AbstractColumnDescriptor object) {
				return createAbstractColumnDescriptorAdapter();
			}
			@Override
			public Adapter casePathSelector(PathSelector object) {
				return createPathSelectorAdapter();
			}
			@Override
			public Adapter caseDynamicProvider(DynamicProvider object) {
				return createDynamicProviderAdapter();
			}
			@Override
			public Adapter caseColumnDescriptor(ColumnDescriptor object) {
				return createColumnDescriptorAdapter();
			}
			@Override
			public Adapter caseWeightedColumnDescriptor(WeightedColumnDescriptor object) {
				return createWeightedColumnDescriptorAdapter();
			}
			@Override
			public Adapter caseStyleRule(StyleRule object) {
				return createStyleRuleAdapter();
			}
			@Override
			public Adapter caseParameterized(Parameterized object) {
				return createParameterizedAdapter();
			}
			@Override
			public Adapter defaultCase(EObject object) {
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule <em>Look And Feel Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule
	 * @generated
	 */
	public Adapter createLookAndFeelRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor <em>Abstract Column Descriptor</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor
	 * @generated
	 */
	public Adapter createAbstractColumnDescriptorAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector <em>Path Selector</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.PathSelector
	 * @generated
	 */
	public Adapter createPathSelectorAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider <em>Dynamic Provider</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider
	 * @generated
	 */
	public Adapter createDynamicProviderAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor <em>Column Descriptor</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor
	 * @generated
	 */
	public Adapter createColumnDescriptorAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor <em>Weighted Column Descriptor</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor
	 * @generated
	 */
	public Adapter createWeightedColumnDescriptorAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.StyleRule <em>Style Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.StyleRule
	 * @generated
	 */
	public Adapter createStyleRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.edp.handlers.Parameterized <em>Parameterized</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.edp.handlers.Parameterized
	 * @generated
	 */
	public Adapter createParameterizedAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

} //CoreCollectionsStylesAdapterFactory
