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

import org.eclipse.wazaabi.mm.core.Alignment;

import org.eclipse.wazaabi.mm.core.extras.CellEditor;

import org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;

import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Abstract Column Descriptor</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl#getParameters <em>Parameters</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl#getEditingSupport <em>Editing Support</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl#getCellEditor <em>Cell Editor</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl#isResizable <em>Resizable</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.collections.impl.AbstractColumnDescriptorImpl#getHeaderAlignment <em>Header Alignment</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class AbstractColumnDescriptorImpl extends EObjectImpl implements AbstractColumnDescriptor {
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
     * The cached value of the '{@link #getCellEditor() <em>Cell Editor</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getCellEditor()
     * @generated
     * @ordered
     */
	protected CellEditor cellEditor;

	/**
     * The default value of the '{@link #isResizable() <em>Resizable</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isResizable()
     * @generated
     * @ordered
     */
	protected static final boolean RESIZABLE_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isResizable() <em>Resizable</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isResizable()
     * @generated
     * @ordered
     */
	protected boolean resizable = RESIZABLE_EDEFAULT;

	/**
     * The default value of the '{@link #getHeaderAlignment() <em>Header Alignment</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHeaderAlignment()
     * @generated
     * @ordered
     */
	protected static final Alignment HEADER_ALIGNMENT_EDEFAULT = Alignment.LEAD;

	/**
     * The cached value of the '{@link #getHeaderAlignment() <em>Header Alignment</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHeaderAlignment()
     * @generated
     * @ordered
     */
	protected Alignment headerAlignment = HEADER_ALIGNMENT_EDEFAULT;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected AbstractColumnDescriptorImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreCollectionsStylesPackage.Literals.ABSTRACT_COLUMN_DESCRIPTOR;
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
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME, oldPropertyName, propertyName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EList<Parameter> getParameters() {
        if (parameters == null) {
            parameters = new EObjectContainmentEList<Parameter>(Parameter.class, this, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS);
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
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__LABEL, oldLabel, label));
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
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT, oldEditingSupport, editingSupport));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public CellEditor getCellEditor() {
        return cellEditor;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public NotificationChain basicSetCellEditor(CellEditor newCellEditor, NotificationChain msgs) {
        CellEditor oldCellEditor = cellEditor;
        cellEditor = newCellEditor;
        if (eNotificationRequired()) {
            ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR, oldCellEditor, newCellEditor);
            if (msgs == null) msgs = notification; else msgs.add(notification);
        }
        return msgs;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setCellEditor(CellEditor newCellEditor) {
        if (newCellEditor != cellEditor) {
            NotificationChain msgs = null;
            if (cellEditor != null)
                msgs = ((InternalEObject)cellEditor).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR, null, msgs);
            if (newCellEditor != null)
                msgs = ((InternalEObject)newCellEditor).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR, null, msgs);
            msgs = basicSetCellEditor(newCellEditor, msgs);
            if (msgs != null) msgs.dispatch();
        }
        else if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR, newCellEditor, newCellEditor));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isResizable() {
        return resizable;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setResizable(boolean newResizable) {
        boolean oldResizable = resizable;
        resizable = newResizable;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE, oldResizable, resizable));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Alignment getHeaderAlignment() {
        return headerAlignment;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setHeaderAlignment(Alignment newHeaderAlignment) {
        Alignment oldHeaderAlignment = headerAlignment;
        headerAlignment = newHeaderAlignment == null ? HEADER_ALIGNMENT_EDEFAULT : newHeaderAlignment;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__HEADER_ALIGNMENT, oldHeaderAlignment, headerAlignment));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
        switch (featureID) {
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS:
                return ((InternalEList<?>)getParameters()).basicRemove(otherEnd, msgs);
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR:
                return basicSetCellEditor(null, msgs);
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
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME:
                return getPropertyName();
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS:
                return getParameters();
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__LABEL:
                return getLabel();
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT:
                return getEditingSupport();
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR:
                return getCellEditor();
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE:
                return isResizable();
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__HEADER_ALIGNMENT:
                return getHeaderAlignment();
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
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME:
                setPropertyName((String)newValue);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS:
                getParameters().clear();
                getParameters().addAll((Collection<? extends Parameter>)newValue);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__LABEL:
                setLabel((String)newValue);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT:
                setEditingSupport((String)newValue);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR:
                setCellEditor((CellEditor)newValue);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE:
                setResizable((Boolean)newValue);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__HEADER_ALIGNMENT:
                setHeaderAlignment((Alignment)newValue);
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
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME:
                setPropertyName(PROPERTY_NAME_EDEFAULT);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS:
                getParameters().clear();
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__LABEL:
                setLabel(LABEL_EDEFAULT);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT:
                setEditingSupport(EDITING_SUPPORT_EDEFAULT);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR:
                setCellEditor((CellEditor)null);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE:
                setResizable(RESIZABLE_EDEFAULT);
                return;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__HEADER_ALIGNMENT:
                setHeaderAlignment(HEADER_ALIGNMENT_EDEFAULT);
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
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PROPERTY_NAME:
                return PROPERTY_NAME_EDEFAULT == null ? propertyName != null : !PROPERTY_NAME_EDEFAULT.equals(propertyName);
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS:
                return parameters != null && !parameters.isEmpty();
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__LABEL:
                return LABEL_EDEFAULT == null ? label != null : !LABEL_EDEFAULT.equals(label);
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__EDITING_SUPPORT:
                return EDITING_SUPPORT_EDEFAULT == null ? editingSupport != null : !EDITING_SUPPORT_EDEFAULT.equals(editingSupport);
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__CELL_EDITOR:
                return cellEditor != null;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__RESIZABLE:
                return resizable != RESIZABLE_EDEFAULT;
            case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__HEADER_ALIGNMENT:
                return headerAlignment != HEADER_ALIGNMENT_EDEFAULT;
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
                case CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS: return EDPHandlersPackage.PARAMETERIZED__PARAMETERS;
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
                case EDPHandlersPackage.PARAMETERIZED__PARAMETERS: return CoreCollectionsStylesPackage.ABSTRACT_COLUMN_DESCRIPTOR__PARAMETERS;
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
        result.append(", editingSupport: ");
        result.append(editingSupport);
        result.append(", resizable: ");
        result.append(resizable);
        result.append(", headerAlignment: ");
        result.append(headerAlignment);
        result.append(')');
        return result.toString();
    }

} //AbstractColumnDescriptorImpl
