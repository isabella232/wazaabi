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
package org.eclipse.wazaabi.mm.core.widgets;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Container</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.Container#getChildren <em>Children</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getContainer()
 * @model
 * @generated
 */
public interface Container extends AbstractComponent {
	/**
     * Returns the value of the '<em><b>Children</b></em>' containment reference list.
     * The list contents are of type {@link org.eclipse.wazaabi.mm.core.widgets.AbstractComponent}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Children</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Children</em>' containment reference list.
     * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getContainer_Children()
     * @model containment="true"
     *        annotation="http://www.wazaabi.org/Annotation doc='Children about .... .... '"
     * @generated
     */
	EList<AbstractComponent> getChildren();

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='\t\tEList<AbstractComponent> widgets = new org.eclipse.emf.common.util.BasicEList<AbstractComponent>();\r\n\t\tfor(AbstractComponent child : this.getChildren()) {\r\n\t\t  \tif (id.equals(child.getId()))\r\n\t\t   \t\twidgets.add(child);\r\n\t\t   \telse if (child instanceof Container) {\r\n\t\t    \twidgets.addAll(((Container) child).getElementsById(id));\r\n\t\t    }\r\n\t\t}\r\n\t\treturn widgets;'"
     * @generated
     */
	EList<AbstractComponent> getElementsById(String id);

} // Container
