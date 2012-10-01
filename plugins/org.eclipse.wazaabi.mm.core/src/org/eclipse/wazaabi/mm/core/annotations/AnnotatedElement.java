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
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='if (source != null && !\"\".equals(source) && key != null //$NON-NLS-1$\r\n\t\t&& !\"\".equals(key)) { //$NON-NLS-1$\r\n\tAnnotation annotation = null;\r\n\tfor (Annotation _annotation : getAnnotations())\r\n\t\tif (source.equals(_annotation.getSource())) {\r\n\t\t\tannotation = _annotation;\r\n\t\t\tbreak;\r\n\t\t}\r\n\tif (annotation == null) {\r\n\t\tannotation = org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory.eINSTANCE\r\n\t\t\t\t.createAnnotation();\r\n\t\tannotation.setSource(source);\r\n\t\tgetAnnotations().add(annotation);\r\n\t}\r\n\torg.eclipse.wazaabi.mm.core.annotations.AnnotationContent content = null;\r\n\tfor (org.eclipse.wazaabi.mm.core.annotations.AnnotationContent _content : annotation.getContents())\r\n\t\tif (key.equals(_content.getKey())) {\r\n\t\t\tcontent = _content;\r\n\t\t\tbreak;\r\n\t\t}\r\n\tif (content == null) {\r\n\t\tcontent = org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory.eINSTANCE\r\n\t\t\t\t.createAnnotationContent();\r\n\t\tannotation.getContents().add(content);\r\n\t}\r\n\tcontent.setKey(key);\r\n\tcontent.setValue(value);\r\n}'"
	 * @generated
	 */
	void setAnnotation(String source, String key, String value);

} // AnnotatedElement
