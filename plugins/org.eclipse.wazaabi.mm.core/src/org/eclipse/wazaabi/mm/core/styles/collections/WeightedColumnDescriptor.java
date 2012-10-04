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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Weighted Column Descriptor</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getWeight <em>Weight</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getMinimumWidth <em>Minimum Width</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getWeightedColumnDescriptor()
 * @model
 * @generated
 */
public interface WeightedColumnDescriptor extends AbstractColumnDescriptor {
	/**
	 * Returns the value of the '<em><b>Weight</b></em>' attribute.
	 * The default value is <code>"20"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Weight</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Weight</em>' attribute.
	 * @see #setWeight(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getWeightedColumnDescriptor_Weight()
	 * @model default="20"
	 * @generated
	 */
	int getWeight();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getWeight <em>Weight</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Weight</em>' attribute.
	 * @see #getWeight()
	 * @generated
	 */
	void setWeight(int value);

	/**
	 * Returns the value of the '<em><b>Minimum Width</b></em>' attribute.
	 * The default value is <code>"20"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Minimum Width</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Minimum Width</em>' attribute.
	 * @see #setMinimumWidth(int)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getWeightedColumnDescriptor_MinimumWidth()
	 * @model default="20"
	 * @generated
	 */
	int getMinimumWidth();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor#getMinimumWidth <em>Minimum Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Minimum Width</em>' attribute.
	 * @see #getMinimumWidth()
	 * @generated
	 */
	void setMinimumWidth(int value);

} // WeightedColumnDescriptor
