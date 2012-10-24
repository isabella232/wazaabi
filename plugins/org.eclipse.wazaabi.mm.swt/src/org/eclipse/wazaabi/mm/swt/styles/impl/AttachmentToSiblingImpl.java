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
package org.eclipse.wazaabi.mm.swt.styles.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.wazaabi.mm.swt.styles.AttachmentToSibling;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.wazaabi.mm.swt.styles.ToSiblingAlignment;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Attachment To Sibling</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToSiblingImpl#getOffset <em>Offset</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToSiblingImpl#getSiblingId <em>Sibling Id</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.AttachmentToSiblingImpl#getAlignment <em>Alignment</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class AttachmentToSiblingImpl extends EObjectImpl implements AttachmentToSibling {
	/**
	 * The default value of the '{@link #getOffset() <em>Offset</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOffset()
	 * @generated
	 * @ordered
	 */
	protected static final int OFFSET_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getOffset() <em>Offset</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOffset()
	 * @generated
	 * @ordered
	 */
	protected int offset = OFFSET_EDEFAULT;

	/**
	 * The default value of the '{@link #getSiblingId() <em>Sibling Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSiblingId()
	 * @generated
	 * @ordered
	 */
	protected static final String SIBLING_ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSiblingId() <em>Sibling Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSiblingId()
	 * @generated
	 * @ordered
	 */
	protected String siblingId = SIBLING_ID_EDEFAULT;

	/**
	 * The default value of the '{@link #getAlignment() <em>Alignment</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAlignment()
	 * @generated
	 * @ordered
	 */
	protected static final ToSiblingAlignment ALIGNMENT_EDEFAULT = ToSiblingAlignment.DEFAULT;

	/**
	 * The cached value of the '{@link #getAlignment() <em>Alignment</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAlignment()
	 * @generated
	 * @ordered
	 */
	protected ToSiblingAlignment alignment = ALIGNMENT_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AttachmentToSiblingImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return SWTStylesPackage.Literals.ATTACHMENT_TO_SIBLING;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getOffset() {
		return offset;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setOffset(int newOffset) {
		int oldOffset = offset;
		offset = newOffset;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ATTACHMENT_TO_SIBLING__OFFSET, oldOffset, offset));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getSiblingId() {
		return siblingId;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSiblingId(String newSiblingId) {
		String oldSiblingId = siblingId;
		siblingId = newSiblingId;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ATTACHMENT_TO_SIBLING__SIBLING_ID, oldSiblingId, siblingId));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ToSiblingAlignment getAlignment() {
		return alignment;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAlignment(ToSiblingAlignment newAlignment) {
		ToSiblingAlignment oldAlignment = alignment;
		alignment = newAlignment == null ? ALIGNMENT_EDEFAULT : newAlignment;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.ATTACHMENT_TO_SIBLING__ALIGNMENT, oldAlignment, alignment));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__OFFSET:
				return getOffset();
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__SIBLING_ID:
				return getSiblingId();
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__ALIGNMENT:
				return getAlignment();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__OFFSET:
				setOffset((Integer)newValue);
				return;
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__SIBLING_ID:
				setSiblingId((String)newValue);
				return;
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__ALIGNMENT:
				setAlignment((ToSiblingAlignment)newValue);
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
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__OFFSET:
				setOffset(OFFSET_EDEFAULT);
				return;
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__SIBLING_ID:
				setSiblingId(SIBLING_ID_EDEFAULT);
				return;
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__ALIGNMENT:
				setAlignment(ALIGNMENT_EDEFAULT);
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
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__OFFSET:
				return offset != OFFSET_EDEFAULT;
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__SIBLING_ID:
				return SIBLING_ID_EDEFAULT == null ? siblingId != null : !SIBLING_ID_EDEFAULT.equals(siblingId);
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING__ALIGNMENT:
				return alignment != ALIGNMENT_EDEFAULT;
		}
		return super.eIsSet(featureID);
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
		result.append(" (offset: ");
		result.append(offset);
		result.append(", siblingId: ");
		result.append(siblingId);
		result.append(", alignment: ");
		result.append(alignment);
		result.append(')');
		return result.toString();
	}

} //AttachmentToSiblingImpl
