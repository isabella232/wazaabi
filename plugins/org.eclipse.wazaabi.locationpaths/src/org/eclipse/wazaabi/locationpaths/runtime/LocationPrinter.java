/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.locationpaths.runtime;

import java.util.Iterator;

import org.eclipse.wazaabi.locationpaths.model.Axis;
import org.eclipse.wazaabi.locationpaths.model.BinaryExpression;
import org.eclipse.wazaabi.locationpaths.model.Expression;
import org.eclipse.wazaabi.locationpaths.model.IntegerExpression;
import org.eclipse.wazaabi.locationpaths.model.LiteralExpression;
import org.eclipse.wazaabi.locationpaths.model.LocationPath;
import org.eclipse.wazaabi.locationpaths.model.Operator;
import org.eclipse.wazaabi.locationpaths.model.Step;

public class LocationPrinter {

	private boolean expanded = false;

	private static final String NODE_NODE_TYPE_LITERAL = "node"; //$NON-NLS-1$

	private static final String CHILD_AXIS_LITERAL = "child::"; //$NON-NLS-1$

	private static final String DESCENDANT_OR_SELF_AXIS_LITERAL = "descendant-or-self::"; //$NON-NLS-1$

	private static final String ATTRIBUTE_AXIS_LITERAL = "attribute::"; //$NON-NLS-1$

	private static final String REFERENCE_AXIS_LITERAL = "reference::"; //$NON-NLS-1$

	private static final String VARIABLE_AXIS_LITERAL = "variable::"; //$NON-NLS-1$

	private static final String PARENT_AXIS_LITERAL = "parent::"; //$NON-NLS-1$

	private static final String SELF_AXIS_LITERAL = "self::"; //$NON-NLS-1$

	private boolean isDescendantOrSelf(Step step) {
		return step.getAxis() == Axis.DESCENDANT_OR_SELF;
	}

	public boolean isExpanded() {
		return expanded;
	}

	/**
	 * Returns true is the step is descendant-or-self::node(), whose short is //
	 * 
	 * @param step
	 * @return
	 */
	private boolean isSlashSlash(Step step) {
		return isDescendantOrSelf(step) && step.getNameTest() == null;
	}

	public void printAxis(int axis, StringBuffer buffer) {
		if (isExpanded())
			printAxisExpanded(axis, buffer);
		else
			printAxisAbbreviated(axis, buffer);
	}

	public void print(LocationPath locationPath, StringBuffer buffer) {
		if (isExpanded())
			printExpanded(locationPath, buffer);
		else
			printAbbreviated(locationPath, buffer);
	}

	public void print(Step step, StringBuffer buffer) {
		if (isExpanded())
			printExpanded(step, buffer);
		else
			printAbbreviated(step, buffer);
	}

	protected void printAxisAbbreviated(int axis, StringBuffer buffer) {
		switch (axis) {
		case Axis.ATTRIBUTE:
			buffer.append('@');
			break;
		case Axis.REFERENCE:
			buffer.append('&');
			break;
		case Axis.VARIABLE:
			buffer.append('$');
			break;
		case Axis.SELF:
			buffer.append('.');
			break;
		case Axis.PARENT:
			buffer.append("..");
			break;
		}
	}

	protected void printAbbreviated(LocationPath locationPath,
			StringBuffer buffer) {
		for (int i = 0; i < locationPath.getSteps().size(); i++) {
			if (isDescendantOrSelf((Step) locationPath.getSteps().get(i))) {
				if (isSlashSlash((Step) locationPath.getSteps().get(i))) {
					if (i == 0)
						buffer.append('.');
					buffer.append("//"); //$NON-NLS-1$
				} else {
					if (i != 0)
						buffer.append('/');
					buffer.append(DESCENDANT_OR_SELF_AXIS_LITERAL);
				}
			} else if (i != 0
					&& !isSlashSlash((Step) locationPath.getSteps().get(i - 1)))
				buffer.append('/');
			printAbbreviated((Step) locationPath.getSteps().get(i), buffer);
		}

	}

	protected void printAbbreviated(Step step, StringBuffer buffer) {
		printAxisAbbreviated(step.getAxis(), buffer);
		if (step != null) {
			if (step.getNameTest() != null)
				buffer.append(step.getNameTest());
		}
		if (step.getPredicates().size() != 0) {
			Iterator predicateIterator = step.getPredicates().iterator();
			while (predicateIterator.hasNext()) {
				buffer.append('[');
				printAbbreviated((Expression) predicateIterator.next(), buffer);
				buffer.append(']');
			}
		}
	}

	protected void printAxisExpanded(int axis, StringBuffer buffer) {
		switch (axis) {
		case Axis.CHILD:
			buffer.append(CHILD_AXIS_LITERAL);
			break;
		case Axis.DESCENDANT_OR_SELF:
			buffer.append(DESCENDANT_OR_SELF_AXIS_LITERAL);
			break;
		case Axis.ATTRIBUTE:
			buffer.append(ATTRIBUTE_AXIS_LITERAL);
			break;
		case Axis.REFERENCE:
			buffer.append(REFERENCE_AXIS_LITERAL);
			break;
		case Axis.VARIABLE:
			buffer.append(VARIABLE_AXIS_LITERAL);
			break;
		case Axis.PARENT:
			buffer.append(PARENT_AXIS_LITERAL);
			break;
		case Axis.SELF:
			buffer.append(SELF_AXIS_LITERAL);
			break;
		}
	}

	protected void printExpanded(LocationPath locationPath, StringBuffer buffer) {
		for (int i = 0; i < locationPath.getSteps().size(); i++) {
			if (i != 0)
				buffer.append('/');
			printExpanded((Step) locationPath.getSteps().get(i), buffer);
		}
	}

	protected void printExpanded(Step step, StringBuffer buffer) {
		printAxisExpanded(step.getAxis(), buffer);
		if (step.getNameTest() != null)
			buffer.append(step.getNameTest());
		else
			buffer.append(NODE_NODE_TYPE_LITERAL + "()"); //$NON-NLS-1$
		if (step.getPredicates().size() != 0) {
			buffer.append('[');
			Iterator predicateIterator = step.getPredicates().iterator();
			while (predicateIterator.hasNext())
				printExpanded((Expression) predicateIterator.next(), buffer);
			buffer.append(']');
		}
	}

	public void setExpanded(boolean expanded) {
		this.expanded = expanded;
	}

	public String toString(LocationPath locationPath) {
		StringBuffer buffer = new StringBuffer(300);
		print(locationPath, buffer);
		return buffer.toString();
	}

	public void print(Expression expression, StringBuffer buffer) {
		if (isExpanded())
			printExpanded(expression, buffer);
		else
			printAbbreviated(expression, buffer);
	}

	protected void printExpanded(Expression expression, StringBuffer buffer) {
		if (expression instanceof IntegerExpression)
			print((IntegerExpression) expression, buffer);
		else if (expression instanceof BinaryExpression)
			printExpanded(((BinaryExpression) expression), buffer);
		else if (expression instanceof LiteralExpression)
			printExpanded((LiteralExpression) expression, buffer);
		else if (expression instanceof LocationPath)
			printExpanded((LocationPath) expression, buffer);

		// printExpanded(step.getAxis(), buffer);
		// if (step.isSetNameTest()) {
		// if (step.getNameTest() != null)
		// buffer.append(step.getNameTest());
		// } else
		//			buffer.append(NODE_NODE_TYPE_LITERAL + "()"); //$NON-NLS-1$
	}

	protected void printAbbreviated(Expression expression, StringBuffer buffer) {
		if (expression instanceof IntegerExpression)
			print((IntegerExpression) expression, buffer);
		else if (expression instanceof BinaryExpression)
			printAbbreviated((BinaryExpression) expression, buffer);
		else if (expression instanceof LiteralExpression)
			printAbbreviated((LiteralExpression) expression, buffer);
		else if (expression instanceof LocationPath)
			printAbbreviated((LocationPath) expression, buffer);
		// switch (axis.getValue()) {
		// case Axis.ATTRIBUTE_VALUE:
		// buffer.append('@');
		// break;
		// case Axis.REFERENCE_VALUE:
		// buffer.append('&');
		// break;
		// case Axis.SELF_VALUE:
		// buffer.append('.');
		// break;
		// }
	}

	public void print(IntegerExpression integer, StringBuffer buffer) {
		buffer.append(integer.getValue());
	}

	public void print(BinaryExpression binaryExpression, StringBuffer buffer) {
		if (isExpanded())
			printExpanded(binaryExpression, buffer);
		else
			printAbbreviated(binaryExpression, buffer);
	}

	protected void printExpanded(BinaryExpression binaryExpression,
			StringBuffer buffer) {
		printExpanded(binaryExpression.getLeftOperand(), buffer);
		printOperator(binaryExpression.getOperator(), buffer);
		printExpanded(binaryExpression.getRightOperand(), buffer);
	}

	protected void printAbbreviated(BinaryExpression binaryExpression,
			StringBuffer buffer) {
		printAbbreviated(binaryExpression.getLeftOperand(), buffer);
		printOperator(binaryExpression.getOperator(), buffer);
		printAbbreviated(binaryExpression.getRightOperand(), buffer);
	}

	protected void printAbbreviated(LiteralExpression literalExpression,
			StringBuffer buffer) {
		buffer.append("'" + literalExpression.getValue() + "'");
	}

	private void printOperator(int operator, StringBuffer buffer) {
		switch (operator) {
		case Operator.AND:
			buffer.append(" and "); //$NON-NLS-1$
			break;
		case Operator.EQUAL:
			buffer.append("="); //$NON-NLS-1$
			break;
		case Operator.NOT_EQUAL:
			buffer.append("!="); //$NON-NLS-1$
			break;
		case Operator.OR:
			buffer.append(" or "); //$NON-NLS-1$
			break;
		}
	}
}
