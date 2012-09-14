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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Look And Feel</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage#getLookAndFeel()
 * @model
 * @generated
 */
public enum LookAndFeel implements Enumerator {
	/**
	 * The '<em><b>TABLE</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TABLE_VALUE
	 * @generated
	 * @ordered
	 */
	TABLE(0, "TABLE", "TABLE"),

	/**
	 * The '<em><b>TREE</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TREE_VALUE
	 * @generated
	 * @ordered
	 */
	TREE(1, "TREE", "TREE"),

	/**
	 * The '<em><b>COMBOBOX</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #COMBOBOX_VALUE
	 * @generated
	 * @ordered
	 */
	COMBOBOX(2, "COMBOBOX", "COMBOBOX");

	/**
	 * The '<em><b>TABLE</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>TABLE</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #TABLE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int TABLE_VALUE = 0;

	/**
	 * The '<em><b>TREE</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>TREE</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #TREE
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int TREE_VALUE = 1;

	/**
	 * The '<em><b>COMBOBOX</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>COMBOBOX</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #COMBOBOX
	 * @model
	 * @generated
	 * @ordered
	 */
	public static final int COMBOBOX_VALUE = 2;

	/**
	 * An array of all the '<em><b>Look And Feel</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final LookAndFeel[] VALUES_ARRAY =
		new LookAndFeel[] {
			TABLE,
			TREE,
			COMBOBOX,
		};

	/**
	 * A public read-only list of all the '<em><b>Look And Feel</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<LookAndFeel> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Look And Feel</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static LookAndFeel get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			LookAndFeel result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Look And Feel</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static LookAndFeel getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			LookAndFeel result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Look And Feel</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static LookAndFeel get(int value) {
		switch (value) {
			case TABLE_VALUE: return TABLE;
			case TREE_VALUE: return TREE;
			case COMBOBOX_VALUE: return COMBOBOX;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final int value;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String name;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String literal;

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private LookAndFeel(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getValue() {
	  return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
	  return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLiteral() {
	  return literal;
	}

	/**
	 * Returns the literal value of the enumerator, which is its string representation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}
	
} //LookAndFeel
