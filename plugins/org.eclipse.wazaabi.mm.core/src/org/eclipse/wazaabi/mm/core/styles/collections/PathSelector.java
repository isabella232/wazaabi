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

import org.eclipse.emf.common.util.EList;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Path Selector</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getEClassifierName <em>EClassifier Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getContext <em>Context</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getPaths <em>Paths</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getPathSelector()
 * @model
 * @generated
 */
public interface PathSelector extends StyleRule {
	/**
     * Returns the value of the '<em><b>EClassifier Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>EClassifier Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>EClassifier Name</em>' attribute.
     * @see #setEClassifierName(String)
     * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getPathSelector_EClassifierName()
     * @model
     * @generated
     */
	String getEClassifierName();

	/**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getEClassifierName <em>EClassifier Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>EClassifier Name</em>' attribute.
     * @see #getEClassifierName()
     * @generated
     */
	void setEClassifierName(String value);

	/**
     * Returns the value of the '<em><b>Context</b></em>' attribute.
     * The default value is <code>"."</code>.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Context</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Context</em>' attribute.
     * @see #setContext(String)
     * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getPathSelector_Context()
     * @model default="."
     * @generated
     */
	String getContext();

	/**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.PathSelector#getContext <em>Context</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Context</em>' attribute.
     * @see #getContext()
     * @generated
     */
	void setContext(String value);

	/**
     * Returns the value of the '<em><b>Paths</b></em>' attribute list.
     * The list contents are of type {@link java.lang.String}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Paths</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Paths</em>' attribute list.
     * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getPathSelector_Paths()
     * @model
     * @generated
     */
	EList<String> getPaths();

} // PathSelector
