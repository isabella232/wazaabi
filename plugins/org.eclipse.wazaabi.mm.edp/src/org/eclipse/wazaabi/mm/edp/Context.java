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
package org.eclipse.wazaabi.mm.edp;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Context</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.edp.Context#getContents <em>Contents</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.edp.EdpPackage#getContext()
 * @model interface="true" abstract="true"
 * @generated
 */
public interface Context extends EObject
{
  /**
   * Returns the value of the '<em><b>Contents</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.wazaabi.mm.edp.ContextContent}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Contents</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Contents</em>' containment reference list.
   * @see org.eclipse.wazaabi.mm.edp.EdpPackage#getContext_Contents()
   * @model containment="true" transient="true"
   * @generated
   */
  EList<ContextContent> getContents();

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @model keyRequired="true"
   *        annotation="http://www.eclipse.org/emf/2002/GenModel body='return containsKey(key, false);'"
   * @generated
   */
  boolean containsKey(String key);

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @model keyRequired="true"
   *        annotation="http://www.eclipse.org/emf/2002/GenModel body='if (key == null || \"\".equals(key)) //$NON-NLS-1$\r\n\treturn false;\r\nfor (int i = 0; i < getContents().size(); i++) {\r\n\tContextContent content = (ContextContent) getContents().get(i);\r\n\tif (key.equals(content.getKey()))\r\n\t\treturn true;\r\n}\r\nif (!local\r\n\t\t&& eContainer() instanceof org.eclipse.wazaabi.mm.edp.Context)\r\n\treturn ((org.eclipse.wazaabi.mm.edp.Context) eContainer())\r\n\t\t\t.containsKey(key, false);\r\nreturn false;'"
   * @generated
   */
  boolean containsKey(String key, boolean local);

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @model keyRequired="true"
   *        annotation="http://www.eclipse.org/emf/2002/GenModel body='if (key == null || \"\".equals(key)) //$NON-NLS-1$\r\n\treturn null;\r\nfor (int i = 0; i < getContents().size(); i++) {\r\n\tContextContent content = (ContextContent) getContents().get(i);\r\n\tif (key.equals(content.getKey()))\r\n\t\treturn content.getValue();\r\n}\r\nif (eContainer() instanceof org.eclipse.wazaabi.mm.edp.Context)\r\n\treturn ((org.eclipse.wazaabi.mm.edp.Context) eContainer())\r\n\t\t\t.get(key);\r\nreturn null;'"
   * @generated
   */
  Object get(String key);

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @model keyRequired="true"
   *        annotation="http://www.eclipse.org/emf/2002/GenModel body='if (key == null || \"\".equals(key)) //$NON-NLS-1$\r\n\treturn;\r\n\r\nContextContent content = null;\r\nboolean found = false;\r\nfor (int i = 0; i < getContents().size(); i++) {\r\n\tcontent = (ContextContent) getContents().get(i);\r\n\tif (key.equals(content.getKey())) {\r\n\t\tfound = true;\r\n\t\tbreak;\r\n\t}\r\n}\r\nif (found)\r\n\tgetContents().remove(content);'"
   * @generated
   */
  void remove(String key);

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @model keyRequired="true"
   *        annotation="http://www.eclipse.org/emf/2002/GenModel body='if (key == null || \"\".equals(key)) //$NON-NLS-1$\r\n\treturn;\r\n\r\nfor (int i = 0; i < getContents().size(); i++) {\r\n\tContextContent content = (ContextContent) getContents().get(i);\r\n\tif (key.equals(content.getKey())) {\r\n\t\tcontent.setValue(value);\r\n\t\treturn;\r\n\t}\r\n}\r\nContextContent newContent = org.eclipse.wazaabi.mm.edp.EdpFactory.eINSTANCE\r\n\t\t.createContextContent();\r\nnewContent.setKey(key);\r\nnewContent.setValue(value);\r\ngetContents().add(newContent);'"
   * @generated
   */
  void set(String key, Object value);

} // Context
