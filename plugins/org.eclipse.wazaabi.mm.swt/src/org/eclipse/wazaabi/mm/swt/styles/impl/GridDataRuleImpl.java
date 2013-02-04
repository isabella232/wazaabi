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

import org.eclipse.wazaabi.mm.core.styles.impl.LayoutDataRuleImpl;

import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Grid Data Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#isExclude <em>Exclude</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#isGrabExcessHorizontalSpace <em>Grab Excess Horizontal Space</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#isGrabExcessVerticalSpace <em>Grab Excess Vertical Space</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getHeightHint <em>Height Hint</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getHorizontalAlignement <em>Horizontal Alignement</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getHorizontalIndent <em>Horizontal Indent</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getHorizontalSpan <em>Horizontal Span</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getMinimumHeight <em>Minimum Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getMinimumWidth <em>Minimum Width</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getVerticalAlignement <em>Vertical Alignement</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getVerticalIndent <em>Vertical Indent</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getVerticalSpan <em>Vertical Span</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.swt.styles.impl.GridDataRuleImpl#getWidthHint <em>Width Hint</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class GridDataRuleImpl extends LayoutDataRuleImpl implements GridDataRule {
	/**
	 * The default value of the '{@link #isExclude() <em>Exclude</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isExclude()
	 * @generated
	 * @ordered
	 */
	protected static final boolean EXCLUDE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isExclude() <em>Exclude</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isExclude()
	 * @generated
	 * @ordered
	 */
	protected boolean exclude = EXCLUDE_EDEFAULT;

	/**
	 * The default value of the '{@link #isGrabExcessHorizontalSpace() <em>Grab Excess Horizontal Space</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isGrabExcessHorizontalSpace()
	 * @generated
	 * @ordered
	 */
	protected static final boolean GRAB_EXCESS_HORIZONTAL_SPACE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isGrabExcessHorizontalSpace() <em>Grab Excess Horizontal Space</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isGrabExcessHorizontalSpace()
	 * @generated
	 * @ordered
	 */
	protected boolean grabExcessHorizontalSpace = GRAB_EXCESS_HORIZONTAL_SPACE_EDEFAULT;

	/**
	 * The default value of the '{@link #isGrabExcessVerticalSpace() <em>Grab Excess Vertical Space</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isGrabExcessVerticalSpace()
	 * @generated
	 * @ordered
	 */
	protected static final boolean GRAB_EXCESS_VERTICAL_SPACE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isGrabExcessVerticalSpace() <em>Grab Excess Vertical Space</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isGrabExcessVerticalSpace()
	 * @generated
	 * @ordered
	 */
	protected boolean grabExcessVerticalSpace = GRAB_EXCESS_VERTICAL_SPACE_EDEFAULT;

	/**
	 * The default value of the '{@link #getHeightHint() <em>Height Hint</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHeightHint()
	 * @generated
	 * @ordered
	 */
	protected static final int HEIGHT_HINT_EDEFAULT = -1;

	/**
	 * The cached value of the '{@link #getHeightHint() <em>Height Hint</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHeightHint()
	 * @generated
	 * @ordered
	 */
	protected int heightHint = HEIGHT_HINT_EDEFAULT;

	/**
	 * The default value of the '{@link #getHorizontalAlignement() <em>Horizontal Alignement</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHorizontalAlignement()
	 * @generated
	 * @ordered
	 */
	protected static final GridDataAlignment HORIZONTAL_ALIGNEMENT_EDEFAULT = GridDataAlignment.BEGINNING;

	/**
	 * The cached value of the '{@link #getHorizontalAlignement() <em>Horizontal Alignement</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHorizontalAlignement()
	 * @generated
	 * @ordered
	 */
	protected GridDataAlignment horizontalAlignement = HORIZONTAL_ALIGNEMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getHorizontalIndent() <em>Horizontal Indent</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHorizontalIndent()
	 * @generated
	 * @ordered
	 */
	protected static final int HORIZONTAL_INDENT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getHorizontalIndent() <em>Horizontal Indent</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHorizontalIndent()
	 * @generated
	 * @ordered
	 */
	protected int horizontalIndent = HORIZONTAL_INDENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getHorizontalSpan() <em>Horizontal Span</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHorizontalSpan()
	 * @generated
	 * @ordered
	 */
	protected static final int HORIZONTAL_SPAN_EDEFAULT = 1;

	/**
	 * The cached value of the '{@link #getHorizontalSpan() <em>Horizontal Span</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getHorizontalSpan()
	 * @generated
	 * @ordered
	 */
	protected int horizontalSpan = HORIZONTAL_SPAN_EDEFAULT;

	/**
	 * The default value of the '{@link #getMinimumHeight() <em>Minimum Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMinimumHeight()
	 * @generated
	 * @ordered
	 */
	protected static final int MINIMUM_HEIGHT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getMinimumHeight() <em>Minimum Height</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMinimumHeight()
	 * @generated
	 * @ordered
	 */
	protected int minimumHeight = MINIMUM_HEIGHT_EDEFAULT;

	/**
	 * The default value of the '{@link #getMinimumWidth() <em>Minimum Width</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMinimumWidth()
	 * @generated
	 * @ordered
	 */
	protected static final int MINIMUM_WIDTH_EDEFAULT = 0;

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
	 * The default value of the '{@link #getVerticalAlignement() <em>Vertical Alignement</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVerticalAlignement()
	 * @generated
	 * @ordered
	 */
	protected static final GridDataAlignment VERTICAL_ALIGNEMENT_EDEFAULT = GridDataAlignment.BEGINNING;

	/**
	 * The cached value of the '{@link #getVerticalAlignement() <em>Vertical Alignement</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVerticalAlignement()
	 * @generated
	 * @ordered
	 */
	protected GridDataAlignment verticalAlignement = VERTICAL_ALIGNEMENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getVerticalIndent() <em>Vertical Indent</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVerticalIndent()
	 * @generated
	 * @ordered
	 */
	protected static final int VERTICAL_INDENT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getVerticalIndent() <em>Vertical Indent</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVerticalIndent()
	 * @generated
	 * @ordered
	 */
	protected int verticalIndent = VERTICAL_INDENT_EDEFAULT;

	/**
	 * The default value of the '{@link #getVerticalSpan() <em>Vertical Span</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVerticalSpan()
	 * @generated
	 * @ordered
	 */
	protected static final int VERTICAL_SPAN_EDEFAULT = 1;

	/**
	 * The cached value of the '{@link #getVerticalSpan() <em>Vertical Span</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVerticalSpan()
	 * @generated
	 * @ordered
	 */
	protected int verticalSpan = VERTICAL_SPAN_EDEFAULT;

	/**
	 * The default value of the '{@link #getWidthHint() <em>Width Hint</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getWidthHint()
	 * @generated
	 * @ordered
	 */
	protected static final int WIDTH_HINT_EDEFAULT = -1;

	/**
	 * The cached value of the '{@link #getWidthHint() <em>Width Hint</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getWidthHint()
	 * @generated
	 * @ordered
	 */
	protected int widthHint = WIDTH_HINT_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected GridDataRuleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return SWTStylesPackage.Literals.GRID_DATA_RULE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isExclude() {
		return exclude;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setExclude(boolean newExclude) {
		boolean oldExclude = exclude;
		exclude = newExclude;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__EXCLUDE, oldExclude, exclude));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isGrabExcessHorizontalSpace() {
		return grabExcessHorizontalSpace;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGrabExcessHorizontalSpace(boolean newGrabExcessHorizontalSpace) {
		boolean oldGrabExcessHorizontalSpace = grabExcessHorizontalSpace;
		grabExcessHorizontalSpace = newGrabExcessHorizontalSpace;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE, oldGrabExcessHorizontalSpace, grabExcessHorizontalSpace));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isGrabExcessVerticalSpace() {
		return grabExcessVerticalSpace;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setGrabExcessVerticalSpace(boolean newGrabExcessVerticalSpace) {
		boolean oldGrabExcessVerticalSpace = grabExcessVerticalSpace;
		grabExcessVerticalSpace = newGrabExcessVerticalSpace;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE, oldGrabExcessVerticalSpace, grabExcessVerticalSpace));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getHeightHint() {
		return heightHint;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setHeightHint(int newHeightHint) {
		int oldHeightHint = heightHint;
		heightHint = newHeightHint;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__HEIGHT_HINT, oldHeightHint, heightHint));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GridDataAlignment getHorizontalAlignement() {
		return horizontalAlignement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setHorizontalAlignement(GridDataAlignment newHorizontalAlignement) {
		GridDataAlignment oldHorizontalAlignement = horizontalAlignement;
		horizontalAlignement = newHorizontalAlignement == null ? HORIZONTAL_ALIGNEMENT_EDEFAULT : newHorizontalAlignement;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT, oldHorizontalAlignement, horizontalAlignement));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getHorizontalIndent() {
		return horizontalIndent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setHorizontalIndent(int newHorizontalIndent) {
		int oldHorizontalIndent = horizontalIndent;
		horizontalIndent = newHorizontalIndent;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_INDENT, oldHorizontalIndent, horizontalIndent));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getHorizontalSpan() {
		return horizontalSpan;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setHorizontalSpan(int newHorizontalSpan) {
		int oldHorizontalSpan = horizontalSpan;
		horizontalSpan = newHorizontalSpan;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_SPAN, oldHorizontalSpan, horizontalSpan));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getMinimumHeight() {
		return minimumHeight;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setMinimumHeight(int newMinimumHeight) {
		int oldMinimumHeight = minimumHeight;
		minimumHeight = newMinimumHeight;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__MINIMUM_HEIGHT, oldMinimumHeight, minimumHeight));
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
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__MINIMUM_WIDTH, oldMinimumWidth, minimumWidth));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public GridDataAlignment getVerticalAlignement() {
		return verticalAlignement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVerticalAlignement(GridDataAlignment newVerticalAlignement) {
		GridDataAlignment oldVerticalAlignement = verticalAlignement;
		verticalAlignement = newVerticalAlignement == null ? VERTICAL_ALIGNEMENT_EDEFAULT : newVerticalAlignement;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__VERTICAL_ALIGNEMENT, oldVerticalAlignement, verticalAlignement));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getVerticalIndent() {
		return verticalIndent;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVerticalIndent(int newVerticalIndent) {
		int oldVerticalIndent = verticalIndent;
		verticalIndent = newVerticalIndent;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__VERTICAL_INDENT, oldVerticalIndent, verticalIndent));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getVerticalSpan() {
		return verticalSpan;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVerticalSpan(int newVerticalSpan) {
		int oldVerticalSpan = verticalSpan;
		verticalSpan = newVerticalSpan;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__VERTICAL_SPAN, oldVerticalSpan, verticalSpan));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getWidthHint() {
		return widthHint;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setWidthHint(int newWidthHint) {
		int oldWidthHint = widthHint;
		widthHint = newWidthHint;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, SWTStylesPackage.GRID_DATA_RULE__WIDTH_HINT, oldWidthHint, widthHint));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case SWTStylesPackage.GRID_DATA_RULE__EXCLUDE:
				return isExclude();
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE:
				return isGrabExcessHorizontalSpace();
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE:
				return isGrabExcessVerticalSpace();
			case SWTStylesPackage.GRID_DATA_RULE__HEIGHT_HINT:
				return getHeightHint();
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT:
				return getHorizontalAlignement();
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_INDENT:
				return getHorizontalIndent();
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_SPAN:
				return getHorizontalSpan();
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_HEIGHT:
				return getMinimumHeight();
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_WIDTH:
				return getMinimumWidth();
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_ALIGNEMENT:
				return getVerticalAlignement();
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_INDENT:
				return getVerticalIndent();
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_SPAN:
				return getVerticalSpan();
			case SWTStylesPackage.GRID_DATA_RULE__WIDTH_HINT:
				return getWidthHint();
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
			case SWTStylesPackage.GRID_DATA_RULE__EXCLUDE:
				setExclude((Boolean)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE:
				setGrabExcessHorizontalSpace((Boolean)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE:
				setGrabExcessVerticalSpace((Boolean)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HEIGHT_HINT:
				setHeightHint((Integer)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT:
				setHorizontalAlignement((GridDataAlignment)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_INDENT:
				setHorizontalIndent((Integer)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_SPAN:
				setHorizontalSpan((Integer)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_HEIGHT:
				setMinimumHeight((Integer)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_WIDTH:
				setMinimumWidth((Integer)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_ALIGNEMENT:
				setVerticalAlignement((GridDataAlignment)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_INDENT:
				setVerticalIndent((Integer)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_SPAN:
				setVerticalSpan((Integer)newValue);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__WIDTH_HINT:
				setWidthHint((Integer)newValue);
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
			case SWTStylesPackage.GRID_DATA_RULE__EXCLUDE:
				setExclude(EXCLUDE_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE:
				setGrabExcessHorizontalSpace(GRAB_EXCESS_HORIZONTAL_SPACE_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE:
				setGrabExcessVerticalSpace(GRAB_EXCESS_VERTICAL_SPACE_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HEIGHT_HINT:
				setHeightHint(HEIGHT_HINT_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT:
				setHorizontalAlignement(HORIZONTAL_ALIGNEMENT_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_INDENT:
				setHorizontalIndent(HORIZONTAL_INDENT_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_SPAN:
				setHorizontalSpan(HORIZONTAL_SPAN_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_HEIGHT:
				setMinimumHeight(MINIMUM_HEIGHT_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_WIDTH:
				setMinimumWidth(MINIMUM_WIDTH_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_ALIGNEMENT:
				setVerticalAlignement(VERTICAL_ALIGNEMENT_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_INDENT:
				setVerticalIndent(VERTICAL_INDENT_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_SPAN:
				setVerticalSpan(VERTICAL_SPAN_EDEFAULT);
				return;
			case SWTStylesPackage.GRID_DATA_RULE__WIDTH_HINT:
				setWidthHint(WIDTH_HINT_EDEFAULT);
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
			case SWTStylesPackage.GRID_DATA_RULE__EXCLUDE:
				return exclude != EXCLUDE_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_HORIZONTAL_SPACE:
				return grabExcessHorizontalSpace != GRAB_EXCESS_HORIZONTAL_SPACE_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__GRAB_EXCESS_VERTICAL_SPACE:
				return grabExcessVerticalSpace != GRAB_EXCESS_VERTICAL_SPACE_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__HEIGHT_HINT:
				return heightHint != HEIGHT_HINT_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_ALIGNEMENT:
				return horizontalAlignement != HORIZONTAL_ALIGNEMENT_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_INDENT:
				return horizontalIndent != HORIZONTAL_INDENT_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__HORIZONTAL_SPAN:
				return horizontalSpan != HORIZONTAL_SPAN_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_HEIGHT:
				return minimumHeight != MINIMUM_HEIGHT_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__MINIMUM_WIDTH:
				return minimumWidth != MINIMUM_WIDTH_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_ALIGNEMENT:
				return verticalAlignement != VERTICAL_ALIGNEMENT_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_INDENT:
				return verticalIndent != VERTICAL_INDENT_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__VERTICAL_SPAN:
				return verticalSpan != VERTICAL_SPAN_EDEFAULT;
			case SWTStylesPackage.GRID_DATA_RULE__WIDTH_HINT:
				return widthHint != WIDTH_HINT_EDEFAULT;
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
		result.append(" (exclude: ");
		result.append(exclude);
		result.append(", grabExcessHorizontalSpace: ");
		result.append(grabExcessHorizontalSpace);
		result.append(", grabExcessVerticalSpace: ");
		result.append(grabExcessVerticalSpace);
		result.append(", heightHint: ");
		result.append(heightHint);
		result.append(", horizontalAlignement: ");
		result.append(horizontalAlignement);
		result.append(", horizontalIndent: ");
		result.append(horizontalIndent);
		result.append(", horizontalSpan: ");
		result.append(horizontalSpan);
		result.append(", minimumHeight: ");
		result.append(minimumHeight);
		result.append(", minimumWidth: ");
		result.append(minimumWidth);
		result.append(", verticalAlignement: ");
		result.append(verticalAlignement);
		result.append(", verticalIndent: ");
		result.append(verticalIndent);
		result.append(", verticalSpan: ");
		result.append(verticalSpan);
		result.append(", widthHint: ");
		result.append(widthHint);
		result.append(')');
		return result.toString();
	}

} //GridDataRuleImpl
