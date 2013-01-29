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
 * A representation of the model object '<em><b>Menu Component</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.MenuComponent#getChildren <em>Children</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.MenuComponent#getText <em>Text</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.MenuComponent#isEnabled <em>Enabled</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getMenuComponent()
 * @model annotation="http://www.wazaabi.org/style/property/definition name='direction' type='package=http://www.wazaabi.org/core/styles\r\nEClass=DirectionRule' default='value=LEFT_TO_RIGHT'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='image' type='package=http://www.wazaabi.org/core/styles\r\nEClass=ImageRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='type' type='package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule' default='value=push'"
 * @generated
 */
public interface MenuComponent extends Widget {
	/**
	 * Returns the value of the '<em><b>Children</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.wazaabi.mm.core.widgets.MenuComponent}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Children</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Children</em>' containment reference list.
	 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getMenuComponent_Children()
	 * @model containment="true"
	 *        annotation="http://www.wazaabi.org/Annotation doc='Children about .... .... '"
	 * @generated
	 */
	EList<MenuComponent> getChildren();

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
	 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getMenuComponent_Text()
	 * @model
	 * @generated
	 */
	String getText();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.widgets.MenuComponent#getText <em>Text</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Text</em>' attribute.
	 * @see #getText()
	 * @generated
	 */
	void setText(String value);

	/**
	 * Returns the value of the '<em><b>Enabled</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Enabled</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Enabled</em>' attribute.
	 * @see #setEnabled(boolean)
	 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getMenuComponent_Enabled()
	 * @model
	 * @generated
	 */
	boolean isEnabled();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.widgets.MenuComponent#isEnabled <em>Enabled</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Enabled</em>' attribute.
	 * @see #isEnabled()
	 * @generated
	 */
	void setEnabled(boolean value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\n\t\t\t\t\"type\", //$NON-NLS-1$\n\t\t\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\n\t\tif (rule == null) {\n\t\t\trule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE\n\t\t\t\t\t.createStringRule();\n\t\t\trule.setPropertyName(\"type\"); //$NON-NLS-1$\n\t\t\tgetStyleRules().add(rule);\n\t\t}\n\t\trule.setValue(text);'"
	 * @generated
	 */
	void setType(String type);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 *        annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\n\t\t\t\"type\", //$NON-NLS-1$\n\t\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\n\t\tif (rule != null)\n\t\t\treturn rule.getValue();\n\treturn null;'"
	 * @generated
	 */
	String getType();

} // MenuComponent
