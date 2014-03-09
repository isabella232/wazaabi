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
package org.eclipse.wazaabi.mm.core.styles;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Styled Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.StyledElement#getStyleRules <em>Style Rules</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getStyledElement()
 * @model abstract="true"
 * @generated
 */
public interface StyledElement extends EObject {
	/**
     * Returns the value of the '<em><b>Style Rules</b></em>' containment reference list.
     * The list contents are of type {@link org.eclipse.wazaabi.mm.core.styles.StyleRule}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Style Rules</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Style Rules</em>' containment reference list.
     * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#getStyledElement_StyleRules()
     * @model containment="true"
     * @generated
     */
	EList<StyleRule> getStyleRules();

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='if (propertyName == null || \"\".equals(propertyName)) //$NON-NLS-1$\r\n\treturn null;\r\nfor (StyleRule rule : getStyleRules())\r\n\tif (propertyName.equals(rule.getPropertyName())\r\n\t\t\t&& (eClass == null || (eClass != null && eClass == rule\r\n\t\t\t\t\t.eClass())))\r\n\t\treturn rule;\r\nreturn null;'"
     * @generated
     */
	StyleRule getFirstStyleRule(String propertyName, EClass eClass);

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='if (propertyName == null || \"\".equals(propertyName)) //$NON-NLS-1$\r\n\treturn;\r\nStyleRule toRemove = null;\r\nfor (StyleRule rule : getStyleRules())\r\n\tif (propertyName.equals(rule.getPropertyName())\r\n\t\t\t&& ((eClass != null && eClass == rule.eClass()) || eClass == null)) {\r\n\t\ttoRemove = rule;\r\n\t\tbreak;\r\n\t}\r\nif (toRemove != null)\r\n\tgetStyleRules().remove(toRemove);'"
     * @generated
     */
	void removeFirstStyleRule(String propertyName, EClass eClass);

} // StyledElement
