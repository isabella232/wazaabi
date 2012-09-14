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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsFactory
 * @model kind="package"
 * @generated
 */
public interface CoreAnnotationsPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "annotations";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.wazaabi.org/core/annotations";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "wca";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CoreAnnotationsPackage eINSTANCE = org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl.init();

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement <em>Annotated Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement
	 * @see org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl#getAnnotatedElement()
	 * @generated
	 */
	int ANNOTATED_ELEMENT = 0;

	/**
	 * The feature id for the '<em><b>Annotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATED_ELEMENT__ANNOTATIONS = 0;

	/**
	 * The number of structural features of the '<em>Annotated Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATED_ELEMENT_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationImpl <em>Annotation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationImpl
	 * @see org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl#getAnnotation()
	 * @generated
	 */
	int ANNOTATION = 1;

	/**
	 * The feature id for the '<em><b>Source</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION__SOURCE = 0;

	/**
	 * The feature id for the '<em><b>Contents</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION__CONTENTS = 1;

	/**
	 * The number of structural features of the '<em>Annotation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationContentImpl <em>Annotation Content</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationContentImpl
	 * @see org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl#getAnnotationContent()
	 * @generated
	 */
	int ANNOTATION_CONTENT = 2;

	/**
	 * The feature id for the '<em><b>Key</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_CONTENT__KEY = 0;

	/**
	 * The feature id for the '<em><b>Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_CONTENT__VALUE = 1;

	/**
	 * The number of structural features of the '<em>Annotation Content</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ANNOTATION_CONTENT_FEATURE_COUNT = 2;


	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement <em>Annotated Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotated Element</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement
	 * @generated
	 */
	EClass getAnnotatedElement();

	/**
	 * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement#getAnnotations <em>Annotations</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Annotations</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement#getAnnotations()
	 * @see #getAnnotatedElement()
	 * @generated
	 */
	EReference getAnnotatedElement_Annotations();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.annotations.Annotation <em>Annotation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotation</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.Annotation
	 * @generated
	 */
	EClass getAnnotation();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.annotations.Annotation#getSource <em>Source</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Source</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.Annotation#getSource()
	 * @see #getAnnotation()
	 * @generated
	 */
	EAttribute getAnnotation_Source();

	/**
	 * Returns the meta object for the containment reference list '{@link org.eclipse.wazaabi.mm.core.annotations.Annotation#getContents <em>Contents</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contents</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.Annotation#getContents()
	 * @see #getAnnotation()
	 * @generated
	 */
	EReference getAnnotation_Contents();

	/**
	 * Returns the meta object for class '{@link org.eclipse.wazaabi.mm.core.annotations.AnnotationContent <em>Annotation Content</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Annotation Content</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.AnnotationContent
	 * @generated
	 */
	EClass getAnnotationContent();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.annotations.AnnotationContent#getKey <em>Key</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Key</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.AnnotationContent#getKey()
	 * @see #getAnnotationContent()
	 * @generated
	 */
	EAttribute getAnnotationContent_Key();

	/**
	 * Returns the meta object for the attribute '{@link org.eclipse.wazaabi.mm.core.annotations.AnnotationContent#getValue <em>Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Value</em>'.
	 * @see org.eclipse.wazaabi.mm.core.annotations.AnnotationContent#getValue()
	 * @see #getAnnotationContent()
	 * @generated
	 */
	EAttribute getAnnotationContent_Value();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CoreAnnotationsFactory getCoreAnnotationsFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement <em>Annotated Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.annotations.AnnotatedElement
		 * @see org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl#getAnnotatedElement()
		 * @generated
		 */
		EClass ANNOTATED_ELEMENT = eINSTANCE.getAnnotatedElement();

		/**
		 * The meta object literal for the '<em><b>Annotations</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANNOTATED_ELEMENT__ANNOTATIONS = eINSTANCE.getAnnotatedElement_Annotations();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationImpl <em>Annotation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationImpl
		 * @see org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl#getAnnotation()
		 * @generated
		 */
		EClass ANNOTATION = eINSTANCE.getAnnotation();

		/**
		 * The meta object literal for the '<em><b>Source</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ANNOTATION__SOURCE = eINSTANCE.getAnnotation_Source();

		/**
		 * The meta object literal for the '<em><b>Contents</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ANNOTATION__CONTENTS = eINSTANCE.getAnnotation_Contents();

		/**
		 * The meta object literal for the '{@link org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationContentImpl <em>Annotation Content</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.eclipse.wazaabi.mm.core.annotations.impl.AnnotationContentImpl
		 * @see org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl#getAnnotationContent()
		 * @generated
		 */
		EClass ANNOTATION_CONTENT = eINSTANCE.getAnnotationContent();

		/**
		 * The meta object literal for the '<em><b>Key</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ANNOTATION_CONTENT__KEY = eINSTANCE.getAnnotationContent_Key();

		/**
		 * The meta object literal for the '<em><b>Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ANNOTATION_CONTENT__VALUE = eINSTANCE.getAnnotationContent_Value();

	}

} //CoreAnnotationsPackage
