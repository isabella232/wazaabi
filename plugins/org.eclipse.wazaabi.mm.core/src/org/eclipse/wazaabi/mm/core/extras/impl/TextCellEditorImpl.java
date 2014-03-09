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
package org.eclipse.wazaabi.mm.core.extras.impl;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;
import org.eclipse.wazaabi.mm.core.extras.TextCellEditor;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Text Cell Editor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * </p>
 *
 * @generated
 */
public class TextCellEditorImpl extends CellEditorImpl implements TextCellEditor {
	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected TextCellEditorImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreExtrasPackage.Literals.TEXT_CELL_EDITOR;
    }

} //TextCellEditorImpl
