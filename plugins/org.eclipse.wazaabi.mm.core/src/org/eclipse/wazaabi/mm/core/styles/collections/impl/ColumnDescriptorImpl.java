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
package org.eclipse.wazaabi.mm.core.styles.collections.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.wazaabi.mm.core.extras.CellEditor;

import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;

import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Column Descriptor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl#getParameters <em>Parameters</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl#getMinimumWidth <em>Minimum Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl#getEditingSupport <em>Editing Support</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.ColumnDescriptorImpl#getCellEditor <em>Cell Editor</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ColumnDescriptorImpl extends EObjectImpl implements ColumnDescriptor {
	/**
	 * The default value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPropertyName()
	 * @generated
	 * @ordered
	 */
	protected static final String PROPERTY_NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPropertyName()
	 * @generated
	 * @ordered
	 */
	protected String propertyName = PROPERTY_NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getParameters() <em>Parameters</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getParameters()
	 * @generated
	 * @ordered
	 */
	protected EList<Parameter> parameters;

	/**
	 * The default value of the '{@link #getLabel() <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected static final String LABEL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLabel() <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected String label = LABEL_EDEFAULT;

	/**
	 * The default value of the '{@link #getMinimumWidth() <em>Minimum Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMinimumWidth()
	 * @generated
	 * @ordered
	 */
	protected static final int MINIMUM_WIDTH_EDEFAULT = 20;

	/**
	 * The cached value of the '{@link #getMinimumWidth() <em>Minimum Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMinimumWidth()
	 * @generated
	 * @ordered
	 */
	protected int minimumWidth = MINIMUM_WIDTH_EDEFAULT;

	/**
	 * The default value of the '{@link #getEditingSupport() <em>Editing Support</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEditingSupport()
	 * @generated
	 * @ordered
	 */
	protected static final String EDITING_SUPPORT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEditingSupport() <em>Editing Support</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEditingSupport()
	 * @generated
	 * @ordered
	 */
	protected String editingSupport = EDITING_SUPPORT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getCellEditor() <em>Cell Editor</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCellEditor()
	 * @generated
	 * @ordered
	 */
	protected CellEditor cellEditor;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ColumnDescriptorImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreCollectionsStylesPackage.Literals.COLUMN_DESCRIPTOR;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getPropertyName() {
		return propertyName;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setPropertyName(String newPropertyName) {
		String oldPropertyName = propertyName;
		propertyName = newPropertyName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PROPERTY_NAME, oldPropertyName, propertyName));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Parameter> getParameters() {
		if (parameters == null) {
			parameters = new EObjectContainmentEList<Parameter>(Parameter.class, this, CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS);
		}
		return parameters;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLabel(String newLabel) {
		String oldLabel = label;
		label = newLabel;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__LABEL, oldLabel, label));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getMinimumWidth() {
		return minimumWidth;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMinimumWidth(int newMinimumWidth) {
		int oldMinimumWidth = minimumWidth;
		minimumWidth = newMinimumWidth;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__MINIMUM_WIDTH, oldMinimumWidth, minimumWidth));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getEditingSupport() {
		return editingSupport;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEditingSupport(String newEditingSupport) {
		String oldEditingSupport = editingSupport;
		editingSupport = newEditingSupport;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__EDITING_SUPPORT, oldEditingSupport, editingSupport));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CellEditor getCellEditor() {
		if (cellEditor != null && cellEditor.eIsProxy()) {
			InternalEObject oldCellEditor = (InternalEObject)cellEditor;
			cellEditor = (CellEditor)eResolveProxy(oldCellEditor);
			if (cellEditor != oldCellEditor) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__CELL_EDITOR, oldCellEditor, cellEditor));
			}
		}
		return cellEditor;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CellEditor basicGetCellEditor() {
		return cellEditor;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setCellEditor(CellEditor newCellEditor) {
		CellEditor oldCellEditor = cellEditor;
		cellEditor = newCellEditor;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__CELL_EDITOR, oldCellEditor, cellEditor));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS:
				return ((InternalEList<?>)getParameters()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PROPERTY_NAME:
				return getPropertyName();
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS:
				return getParameters();
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__LABEL:
				return getLabel();
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				return getMinimumWidth();
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__EDITING_SUPPORT:
				return getEditingSupport();
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__CELL_EDITOR:
				if (resolve) return getCellEditor();
				return basicGetCellEditor();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PROPERTY_NAME:
				setPropertyName((String)newValue);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS:
				getParameters().clear();
				getParameters().addAll((Collection<? extends Parameter>)newValue);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__LABEL:
				setLabel((String)newValue);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				setMinimumWidth((Integer)newValue);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__EDITING_SUPPORT:
				setEditingSupport((String)newValue);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__CELL_EDITOR:
				setCellEditor((CellEditor)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PROPERTY_NAME:
				setPropertyName(PROPERTY_NAME_EDEFAULT);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS:
				getParameters().clear();
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				setMinimumWidth(MINIMUM_WIDTH_EDEFAULT);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__EDITING_SUPPORT:
				setEditingSupport(EDITING_SUPPORT_EDEFAULT);
				return;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__CELL_EDITOR:
				setCellEditor((CellEditor)null);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PROPERTY_NAME:
				return PROPERTY_NAME_EDEFAULT == null ? propertyName != null : !PROPERTY_NAME_EDEFAULT.equals(propertyName);
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS:
				return parameters != null && !parameters.isEmpty();
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__LABEL:
				return LABEL_EDEFAULT == null ? label != null : !LABEL_EDEFAULT.equals(label);
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__MINIMUM_WIDTH:
				return minimumWidth != MINIMUM_WIDTH_EDEFAULT;
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__EDITING_SUPPORT:
				return EDITING_SUPPORT_EDEFAULT == null ? editingSupport != null : !EDITING_SUPPORT_EDEFAULT.equals(editingSupport);
			case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__CELL_EDITOR:
				return cellEditor != null;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int eBaseStructuralFeatureID(int derivedFeatureID, Class<?> baseClass) {
		if (baseClass == Parameterized.class) {
			switch (derivedFeatureID) {
				case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS: return EDPHandlersPackage.PARAMETERIZED__PARAMETERS;
				default: return -1;
			}
		}
		return super.eBaseStructuralFeatureID(derivedFeatureID, baseClass);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int eDerivedStructuralFeatureID(int baseFeatureID, Class<?> baseClass) {
		if (baseClass == Parameterized.class) {
			switch (baseFeatureID) {
				case EDPHandlersPackage.PARAMETERIZED__PARAMETERS: return CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR__PARAMETERS;
				default: return -1;
			}
		}
		return super.eDerivedStructuralFeatureID(baseFeatureID, baseClass);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (propertyName: ");
		result.append(propertyName);
		result.append(", label: ");
		result.append(label);
		result.append(", minimumWidth: ");
		result.append(minimumWidth);
		result.append(", editingSupport: ");
		result.append(editingSupport);
		result.append(')');
		return result.toString();
	}

} //ColumnDescriptorImpl
