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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Text Component</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.TextComponent#getText <em>Text</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getTextComponent()
 * @model annotation="http://www.wazaabi.org/style/property/definition name='horizontal-scrollbar' type='package=http://www.wazaabi.org/core/styles\r\nEClass=ScrollBarRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='vertical-scrollbar' type='package=http://www.wazaabi.org/core/styles\r\nEClass=ScrollBarRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='multi-line' type='package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule'"
 * @generated
 */
public interface TextComponent extends AbstractComponent {
	/**
	 * Returns the value of the '<em><b>Text</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Text</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Text</em>' attribute.
	 * @see #setText(String)
	 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getTextComponent_Text()
	 * @model
	 * @generated
	 */
	String getText();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.widgets.TextComponent#getText <em>Text</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Text</em>' attribute.
	 * @see #getText()
	 * @generated
	 */
	void setText(String value);

} // TextComponent
