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
 * A representation of the model object '<em><b>Annotated Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement#getAnnotations <em>Annotations</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage#getAnnotatedElement()
 * @model interface="true" abstract="true"
 * @generated
 */
public interface AnnotatedElement extends EObject {
	/**
	 * Returns the value of the '<em><b>Annotations</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.wazaabi.mm.core.annotations.Annotation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Annotations</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Annotations</em>' containment reference list.
	 * @see org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage#getAnnotatedElement_Annotations()
	 * @model containment="true"
	 * @generated
	 */
	EList<Annotation> getAnnotations();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model
	 * @generated
	 */
	EList<Annotation> getAnnotations(String source);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model
	 * @generated
	 */
	String getValue(String source, String key);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model
	 * @generated
	 */
	String setValue(String source, String key, String value);

} // AnnotatedElement
