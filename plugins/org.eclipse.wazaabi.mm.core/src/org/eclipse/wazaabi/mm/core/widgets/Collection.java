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
 * A representation of the model object '<em><b>Collection</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.Collection#getInput <em>Input</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.Collection#getSelection <em>Selection</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.Collection#getCheckedElements <em>Checked Elements</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getCollection()
 * @model annotation="http://www.wazaabi.org/style/property/definition name='lookandfeel' type='package=http://www.wazaabi.org/core/styles/collections\r\nEClass=LookAndFeelRule' default='value=TABLE'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='allow-row-selection' type='package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule' default='value=true'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='show-horizontal-lines' type='package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='header-visible' type='package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='checkable' type='package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule' default='value=false'"
 * @generated
 */
public interface Collection extends AbstractComponent {
	/**
     * Returns the value of the '<em><b>Selection</b></em>' attribute list.
     * The list contents are of type {@link java.lang.Object}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selection</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Selection</em>' attribute list.
     * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getCollection_Selection()
     * @model transient="true"
     * @generated
     */
	EList<Object> getSelection();

	/**
     * Returns the value of the '<em><b>Checked Elements</b></em>' attribute list.
     * The list contents are of type {@link java.lang.Object}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Checked Elements</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Checked Elements</em>' attribute list.
     * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getCollection_CheckedElements()
     * @model transient="true"
     * @generated
     */
	EList<Object> getCheckedElements();

	/**
     * Returns the value of the '<em><b>Input</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Input</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Input</em>' attribute.
     * @see #setInput(Object)
     * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getCollection_Input()
     * @model transient="true"
     * @generated
     */
	Object getInput();

	/**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.widgets.Collection#getInput <em>Input</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Input</em>' attribute.
     * @see #getInput()
     * @generated
     */
	void setInput(Object value);

} // Collection
