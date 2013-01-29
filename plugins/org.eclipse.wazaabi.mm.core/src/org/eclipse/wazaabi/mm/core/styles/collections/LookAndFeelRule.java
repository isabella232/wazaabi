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
package org.eclipse.wazaabi.mm.core.styles.collections;

import org.eclipse.wazaabi.mm.core.styles.StyleRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Look And Feel Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule#getValue <em>Value</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getLookAndFeelRule()
 * @model
 * @generated
 */
public interface LookAndFeelRule extends StyleRule {
	/**
	 * Returns the value of the '<em><b>Value</b></em>' attribute.
	 * The default value is <code>"TABLE"</code>.
	 * The literals are from the enumeration {@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Value</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel
	 * @see #setValue(LookAndFeel)
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getLookAndFeelRule_Value()
	 * @model default="TABLE"
	 * @generated
	 */
	LookAndFeel getValue();

	/**
	 * Sets the value of the '{@link org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule#getValue <em>Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Value</em>' attribute.
	 * @see org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel
	 * @see #getValue()
	 * @generated
	 */
	void setValue(LookAndFeel value);

} // LookAndFeelRule
