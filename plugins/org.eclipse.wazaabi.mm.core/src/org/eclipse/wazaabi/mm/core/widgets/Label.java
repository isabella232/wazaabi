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
 * A representation of the model object '<em><b>Label</b></em>'.
 * <!-- end-user-doc -->
 *
 *
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage#getLabel()
 * @model annotation="http://www.wazaabi.org/style/property/definition name='text' type='package=http://www.wazaabi.org/core/styles\r\nEClass=StringRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='image' type='package=http://www.wazaabi.org/core/styles\r\nEClass=ImageRule'"
 *        annotation="http://www.wazaabi.org/style/property/definition name='lookandfeel' type='package=http://www.wazaabi.org/core/styles\r\nEClass=HyperlinkRule'"
 * @generated
 */
public interface Label extends AbstractComponent {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 *        annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.ImageRule rule = (org.eclipse.wazaabi.mm.core.styles.ImageRule) getFirstStyleRule(\r\n\t\t\"image\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.IMAGE_RULE);\r\nif (rule != null)\r\n\treturn rule.getValue();\r\nreturn null;'"
	 * @generated
	 */
	String getImage();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 *        annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\r\n\t\t\"text\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\r\nif (rule != null)\r\n\treturn rule.getValue();\r\nreturn null;'"
	 * @generated
	 */
	String getText();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.ImageRule rule = (org.eclipse.wazaabi.mm.core.styles.ImageRule) getFirstStyleRule(\r\n\t\t\"image\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.IMAGE_RULE);\r\nif (rule == null) {\r\n\trule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE\r\n\t\t\t.createImageRule();\r\n\trule.setPropertyName(\"image\"); //$NON-NLS-1$\r\n\tgetStyleRules().add(rule);\r\n}\r\nrule.setValue(imageUri);'"
	 * @generated
	 */
	void setImage(String imageUri);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model annotation="http://www.eclipse.org/emf/2002/GenModel body='org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(\r\n\t\t\"text\", //$NON-NLS-1$\r\n\t\torg.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);\r\nif (rule == null) {\r\n\trule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE\r\n\t\t\t.createStringRule();\r\n\trule.setPropertyName(\"text\"); //$NON-NLS-1$\r\n\tgetStyleRules().add(rule);\r\n}\r\nrule.setValue(text);'"
	 * @generated
	 */
	void setText(String text);

} // Label
