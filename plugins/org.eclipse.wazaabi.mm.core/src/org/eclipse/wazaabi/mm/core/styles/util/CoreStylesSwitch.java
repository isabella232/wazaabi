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
package org.eclipse.wazaabi.mm.core.styles.util;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

import org.eclipse.wazaabi.mm.core.styles.*;

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
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage
 * @generated
 */
public class CoreStylesSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static CoreStylesPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreStylesSwitch() {
		if (modelPackage == null) {
			modelPackage = CoreStylesPackage.eINSTANCE;
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
			case CoreStylesPackage.STYLED_ELEMENT: {
				StyledElement styledElement = (StyledElement)theEObject;
				T result = caseStyledElement(styledElement);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.STYLE_RULE: {
				StyleRule styleRule = (StyleRule)theEObject;
				T result = caseStyleRule(styleRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.COLOR_RULE: {
				ColorRule colorRule = (ColorRule)theEObject;
				T result = caseColorRule(colorRule);
				if (result == null) result = caseStyleRule(colorRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.STRING_RULE: {
				StringRule stringRule = (StringRule)theEObject;
				T result = caseStringRule(stringRule);
				if (result == null) result = caseStyleRule(stringRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.ORIENTATION_RULE: {
				OrientationRule orientationRule = (OrientationRule)theEObject;
				T result = caseOrientationRule(orientationRule);
				if (result == null) result = caseStyleRule(orientationRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.BOOLEAN_RULE: {
				BooleanRule booleanRule = (BooleanRule)theEObject;
				T result = caseBooleanRule(booleanRule);
				if (result == null) result = caseStyleRule(booleanRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.INT_RULE: {
				IntRule intRule = (IntRule)theEObject;
				T result = caseIntRule(intRule);
				if (result == null) result = caseStyleRule(intRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.BLANK_RULE: {
				BlankRule blankRule = (BlankRule)theEObject;
				T result = caseBlankRule(blankRule);
				if (result == null) result = caseStyleRule(blankRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.FONT_RULE: {
				FontRule fontRule = (FontRule)theEObject;
				T result = caseFontRule(fontRule);
				if (result == null) result = caseStyleRule(fontRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.LAYOUT_RULE: {
				LayoutRule layoutRule = (LayoutRule)theEObject;
				T result = caseLayoutRule(layoutRule);
				if (result == null) result = caseStyleRule(layoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.STACK_LAYOUT_RULE: {
				StackLayoutRule stackLayoutRule = (StackLayoutRule)theEObject;
				T result = caseStackLayoutRule(stackLayoutRule);
				if (result == null) result = caseLayoutRule(stackLayoutRule);
				if (result == null) result = caseStyleRule(stackLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.LAYOUT_DATA_RULE: {
				LayoutDataRule layoutDataRule = (LayoutDataRule)theEObject;
				T result = caseLayoutDataRule(layoutDataRule);
				if (result == null) result = caseStyleRule(layoutDataRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.DIRECTION_RULE: {
				DirectionRule directionRule = (DirectionRule)theEObject;
				T result = caseDirectionRule(directionRule);
				if (result == null) result = caseStyleRule(directionRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.MARKER: {
				Marker marker = (Marker)theEObject;
				T result = caseMarker(marker);
				if (result == null) result = caseStyleRule(marker);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.IMAGE_RULE: {
				ImageRule imageRule = (ImageRule)theEObject;
				T result = caseImageRule(imageRule);
				if (result == null) result = caseStringRule(imageRule);
				if (result == null) result = caseStyleRule(imageRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.TABBED_LAYOUT_RULE: {
				TabbedLayoutRule tabbedLayoutRule = (TabbedLayoutRule)theEObject;
				T result = caseTabbedLayoutRule(tabbedLayoutRule);
				if (result == null) result = caseStackLayoutRule(tabbedLayoutRule);
				if (result == null) result = caseLayoutRule(tabbedLayoutRule);
				if (result == null) result = caseStyleRule(tabbedLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.TAB_RULE: {
				TabRule tabRule = (TabRule)theEObject;
				T result = caseTabRule(tabRule);
				if (result == null) result = caseLayoutDataRule(tabRule);
				if (result == null) result = caseStyleRule(tabRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.BAR_LAYOUT_RULE: {
				BarLayoutRule barLayoutRule = (BarLayoutRule)theEObject;
				T result = caseBarLayoutRule(barLayoutRule);
				if (result == null) result = caseLayoutRule(barLayoutRule);
				if (result == null) result = caseStyleRule(barLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.EXPAND_RULE: {
				ExpandRule expandRule = (ExpandRule)theEObject;
				T result = caseExpandRule(expandRule);
				if (result == null) result = caseLayoutDataRule(expandRule);
				if (result == null) result = caseStyleRule(expandRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.EXPAND_LAYOUT_RULE: {
				ExpandLayoutRule expandLayoutRule = (ExpandLayoutRule)theEObject;
				T result = caseExpandLayoutRule(expandLayoutRule);
				if (result == null) result = caseLayoutRule(expandLayoutRule);
				if (result == null) result = caseStyleRule(expandLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.SASH_FORM_LAYOUT_RULE: {
				SashFormLayoutRule sashFormLayoutRule = (SashFormLayoutRule)theEObject;
				T result = caseSashFormLayoutRule(sashFormLayoutRule);
				if (result == null) result = caseLayoutRule(sashFormLayoutRule);
				if (result == null) result = caseStyleRule(sashFormLayoutRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.HYPERLINK_RULE: {
				HyperlinkRule hyperlinkRule = (HyperlinkRule)theEObject;
				T result = caseHyperlinkRule(hyperlinkRule);
				if (result == null) result = caseLayoutRule(hyperlinkRule);
				if (result == null) result = caseStyleRule(hyperlinkRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case CoreStylesPackage.SASH_RULE: {
				SashRule sashRule = (SashRule)theEObject;
				T result = caseSashRule(sashRule);
				if (result == null) result = caseLayoutDataRule(sashRule);
				if (result == null) result = caseStyleRule(sashRule);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Styled Element</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Styled Element</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseStyledElement(StyledElement object) {
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
	 * Returns the result of interpreting the object as an instance of '<em>Color Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Color Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseColorRule(ColorRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>String Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>String Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseStringRule(StringRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Orientation Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Orientation Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseOrientationRule(OrientationRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Boolean Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Boolean Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseBooleanRule(BooleanRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Int Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Int Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseIntRule(IntRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Blank Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Blank Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseBlankRule(BlankRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Font Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Font Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseFontRule(FontRule object) {
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
	 * Returns the result of interpreting the object as an instance of '<em>Stack Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Stack Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseStackLayoutRule(StackLayoutRule object) {
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
	 * Returns the result of interpreting the object as an instance of '<em>Direction Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Direction Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseDirectionRule(DirectionRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Marker</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Marker</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseMarker(Marker object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Image Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Image Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseImageRule(ImageRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Tabbed Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Tabbed Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseTabbedLayoutRule(TabbedLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Tab Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Tab Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseTabRule(TabRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Bar Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Bar Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseBarLayoutRule(BarLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Expand Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Expand Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseExpandRule(ExpandRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Expand Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Expand Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseExpandLayoutRule(ExpandLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Sash Form Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Sash Form Layout Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSashFormLayoutRule(SashFormLayoutRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Hyperlink Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Hyperlink Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseHyperlinkRule(HyperlinkRule object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Sash Rule</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Sash Rule</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSashRule(SashRule object) {
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

} //CoreStylesSwitch
