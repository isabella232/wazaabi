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

import org.eclipse.wazaabi.mm.core.Direction;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Abstract Component</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.AbstractComponent#getId <em>Id</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.widgets.AbstractComponent#isFocus <em>Focus</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getAbstractComponent()
 * @model abstract="true"
 *        annotation="http://www.wazaabi.org/style/property/definition name='tooltip-text' type='package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='background-color' type='package=http://www.wazaabi.org/core/styles\r\nEClass=ColorRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='foreground-color' type='package=http://www.wazaabi.org/core/styles\r\nEClass=ColorRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='font' type='package=http://www.wazaabi.org/core/styles\r\nEClass=FontRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='direction' type='package=http://www.wazaabi.org/core/styles\r\nEClass=DirectionRule' default='value=LEFT_TO_RIGHT'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='enabled' type='package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule' default='value=true'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='error-text' type='package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='visible' type='package=http://www.wazaabi.org/core/styles\r\nEClass=BooleanRule' default='value=true'"
 * @generated
 */
public interface AbstractComponent extends Widget {
	/**
	 * Returns the value of the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Id</em>' attribute.
	 * @see #setId(String)
	 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getAbstractComponent_Id()
	 * @model id="true"
	 * @generated
	 */
	String getId();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.widgets.AbstractComponent#getId <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Id</em>' attribute.
	 * @see #getId()
	 * @generated
	 */
	void setId(String value);

	/**
	 * Returns the value of the '<em><b>Focus</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Focus</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Focus</em>' attribute.
	 * @see #setFocus(boolean)
	 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getAbstractComponent_Focus()
	 * @model default="false" required="true"
	 * @generated
	 */
	boolean isFocus();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.widgets.AbstractComponent#isFocus <em>Focus</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Focus</em>' attribute.
	 * @see #isFocus()
	 * @generated
	 */
	void setFocus(boolean value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 *        annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.DirectionRule rule = (org.eclipse.wazaabi.mm.core.styles.DirectionRule) getFirstStyleRule(\r\n\t\t\"direction\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.DIRECTION_RULE);\r\nif (rule != null)\r\n\treturn rule.getValue();\r\nreturn null;'"
	 * @generated
	 */
	Direction getDirection();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.DirectionRule rule = (org.eclipse.wazaabi.mm.core.styles.DirectionRule) getFirstStyleRule(\r\n\t\t\"direction\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.DIRECTION_RULE);\r\nif (rule == null) {\r\n\trule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE\r\n\t\t\t.createDirectionRule();\r\n\trule.setPropertyName(\"direction\"); //$NON-NLS-1$\r\n\tgetStyleRules().add(rule);\r\n}\r\nrule.setValue(direction);'"
	 * @generated
	 */
	void setDirection(Direction direction);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 *        annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\r\n\t\t\"tooltip-text\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\r\nif (rule != null)\r\n\treturn rule.getValue();\r\nreturn null;'"
	 * @generated
	 */
	String getToolTipText();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\r\n\t\t\"tooltip-text\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\r\nif (rule == null) {\r\n\trule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE\r\n\t\t\t.createStringRule();\r\n\trule.setPropertyName(\"tooltip-text\"); //$NON-NLS-1$\r\n\tgetStyleRules().add(rule);\r\n}\r\nrule.setValue(text);'"
	 * @generated
	 */
	void setToolTipText(String text);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 *        annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\r\n\t\t\"error-text\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\r\nif (rule != null)\r\n\treturn rule.getValue();\r\nreturn null;'"
	 * @generated
	 */
	String getErrorText();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='if (text == null || \"\".equals(text))\r\n\tremoveFirstStyleRule(\r\n\t\t\t\"error-text\", org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE); //$NON-NLS-1$\r\nelse {\r\n\torg.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\r\n\t\t\t\"error-text\", //$NON-NLS-1$\r\n\t\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\r\n\tif (rule == null) {\r\n\t\trule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE\r\n\t\t\t\t.createStringRule();\r\n\t\trule.setPropertyName(\"error-text\"); //$NON-NLS-1$\r\n\t\tgetStyleRules().add(rule);\r\n\t}\r\n\trule.setValue(text);\r\n}'"
	 * @generated
	 */
	void setErrorText(String text);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='if (enabled)\r\n\tremoveFirstStyleRule(\r\n\t\t\t\"enabled\", org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.BOOLEAN_RULE); //$NON-NLS-1$\r\nelse {\r\n\torg.eclipse.wazaabi.mm.core.styles.BooleanRule rule = (org.eclipse.wazaabi.mm.core.styles.BooleanRule) getFirstStyleRule(\r\n\t\t\t\"enabled\", //$NON-NLS-1$\r\n\t\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.BOOLEAN_RULE);\r\n\tif (rule == null) {\r\n\t\trule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE\r\n\t\t\t\t.createBooleanRule();\r\n\t\trule.setPropertyName(\"enabled\"); //$NON-NLS-1$\r\n\t\tgetStyleRules().add(rule);\r\n\t}\r\n\trule.setValue(false);\r\n}'"
	 * @generated
	 */
	void setEnabled(boolean enabled);

} // AbstractComponent
