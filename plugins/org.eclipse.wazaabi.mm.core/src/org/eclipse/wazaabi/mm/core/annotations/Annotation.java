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
package org.eclipse.wazaabi.mm.core.annotations;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Annotation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.annotations.Annotation#getSource <em>Source</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.annotations.Annotation#getContents <em>Contents</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage#getAnnotation()
 * @model
 * @generated
 */
public interface Annotation extends EObject {
	/**
     * Returns the value of the '<em><b>Source</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Source</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Source</em>' attribute.
     * @see #setSource(String)
     * @see org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage#getAnnotation_Source()
     * @model
     * @generated
     */
	String getSource();

	/**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.annotations.Annotation#getSource <em>Source</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Source</em>' attribute.
     * @see #getSource()
     * @generated
     */
	void setSource(String value);

	/**
     * Returns the value of the '<em><b>Contents</b></em>' containment reference list.
     * The list contents are of type {@link org.eclipse.wazaabi.mm.core.annotations.AnnotationContent}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contents</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Contents</em>' containment reference list.
     * @see org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage#getAnnotation_Contents()
     * @model containment="true"
     * @generated
     */
	EList<AnnotationContent> getContents();

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @model
     * @generated
     */
	String getValue(String key);

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @model
     * @generated
     */
	String setValue(String key, String value);

} // Annotation
