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
package org.eclipse.wazaabi.mm.swt.styles.util;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;

import org.eclipse.wazaabi.mm.swt.styles.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage
 * @generated
 */
public class SWTStylesSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static SWTStylesPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SWTStylesSwitch() {
		if (modelPackage == null) {
			modelPackage = SWTStylesPackage.eINSTANCE;
		}
	}

	/**
	 * Checks whether this is a switch for the given package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @parameter ePackage the package in question.
	 * @return whether this is a switch for the given package.
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/**
	 * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the first non-null result returned by a <code>caseXXX</code> call.
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case SWTStylesPackage.ROW_LAYOUT_RULE: {
				RowLayoutRule rowLayoutRule = (RowLayoutRule)theEObject;
				T result = caseRowLayoutRule(rowLayoutRule);
				if (result == null) result = caseLayoutRule(rowLayoutRule);
				if (result == null) result = caseStyleRule(rowLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.ROW_DATA_RULE: {
				RowDataRule rowDataRule = (RowDataRule)theEObject;
				T result = caseRowDataRule(rowDataRule);
				if (result == null) result = caseLayoutDataRule(rowDataRule);
				if (result == null) result = caseStyleRule(rowDataRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.GRID_LAYOUT_RULE: {
				GridLayoutRule gridLayoutRule = (GridLayoutRule)theEObject;
				T result = caseGridLayoutRule(gridLayoutRule);
				if (result == null) result = caseLayoutRule(gridLayoutRule);
				if (result == null) result = caseStyleRule(gridLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.GRID_DATA_RULE: {
				GridDataRule gridDataRule = (GridDataRule)theEObject;
				T result = caseGridDataRule(gridDataRule);
				if (result == null) result = caseLayoutDataRule(gridDataRule);
				if (result == null) result = caseStyleRule(gridDataRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.FILL_LAYOUT_RULE: {
				FillLayoutRule fillLayoutRule = (FillLayoutRule)theEObject;
				T result = caseFillLayoutRule(fillLayoutRule);
				if (result == null) result = caseLayoutRule(fillLayoutRule);
				if (result == null) result = caseStyleRule(fillLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.FORM_ATTACHMENT: {
				FormAttachment formAttachment = (FormAttachment)theEObject;
				T result = caseFormAttachment(formAttachment);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.ATTACHMENT_TO_SIBLING: {
				AttachmentToSibling attachmentToSibling = (AttachmentToSibling)theEObject;
				T result = caseAttachmentToSibling(attachmentToSibling);
				if (result == null) result = caseFormAttachment(attachmentToSibling);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.ATTACHMENT_TO_CONTAINER: {
				AttachmentToContainer attachmentToContainer = (AttachmentToContainer)theEObject;
				T result = caseAttachmentToContainer(attachmentToContainer);
				if (result == null) result = caseFormAttachment(attachmentToContainer);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.FORM_DATA_RULE: {
				FormDataRule formDataRule = (FormDataRule)theEObject;
				T result = caseFormDataRule(formDataRule);
				if (result == null) result = caseLayoutDataRule(formDataRule);
				if (result == null) result = caseStyleRule(formDataRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SWTStylesPackage.FORM_LAYOUT_RULE: {
				FormLayoutRule formLayoutRule = (FormLayoutRule)theEObject;
				T result = caseFormLayoutRule(formLayoutRule);
				if (result == null) result = caseLayoutRule(formLayoutRule);
				if (result == null) result = caseStyleRule(formLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Row Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Row Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseRowLayoutRule(RowLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Row Data Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Row Data Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseRowDataRule(RowDataRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Grid Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Grid Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseGridLayoutRule(GridLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Grid Data Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Grid Data Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseGridDataRule(GridDataRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Fill Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Fill Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFillLayoutRule(FillLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Form Attachment</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Form Attachment</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFormAttachment(FormAttachment object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Attachment To Sibling</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Attachment To Sibling</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAttachmentToSibling(AttachmentToSibling object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Attachment To Container</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Attachment To Container</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAttachmentToContainer(AttachmentToContainer object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Form Data Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Form Data Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFormDataRule(FormDataRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Form Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Form Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFormLayoutRule(FormLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Style Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Style Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseStyleRule(StyleRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLayoutRule(LayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Layout Data Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Layout Data Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLayoutDataRule(LayoutDataRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch, but this is the last case anyway.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

} //SWTStylesSwitch
