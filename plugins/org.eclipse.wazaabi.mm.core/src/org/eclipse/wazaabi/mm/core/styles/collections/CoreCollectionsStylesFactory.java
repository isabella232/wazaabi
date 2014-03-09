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
package org.eclipse.wazaabi.mm.core.styles.collections;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage
 * @generated
 */
public interface CoreCollectionsStylesFactory extends EFactory {
	/**
     * The singleton instance of the factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	CoreCollectionsStylesFactory eINSTANCE = org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesFactoryImpl.init();

	/**
     * Returns a new object of class '<em>Look And Feel Rule</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Look And Feel Rule</em>'.
     * @generated
     */
	LookAndFeelRule createLookAndFeelRule();

	/**
     * Returns a new object of class '<em>Path Selector</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Path Selector</em>'.
     * @generated
     */
	PathSelector createPathSelector();

	/**
     * Returns a new object of class '<em>Dynamic Provider</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Dynamic Provider</em>'.
     * @generated
     */
	DynamicProvider createDynamicProvider();

	/**
     * Returns a new object of class '<em>Column Descriptor</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Column Descriptor</em>'.
     * @generated
     */
	ColumnDescriptor createColumnDescriptor();

	/**
     * Returns a new object of class '<em>Weighted Column Descriptor</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Weighted Column Descriptor</em>'.
     * @generated
     */
	WeightedColumnDescriptor createWeightedColumnDescriptor();

	/**
     * Returns the package supported by this factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the package supported by this factory.
     * @generated
     */
	CoreCollectionsStylesPackage getCoreCollectionsStylesPackage();

} //CoreCollectionsStylesFactory
