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

package org.eclipse.wazaabi.engine.locationpaths.model;

public class BinaryExpression extends Expression {

	public BinaryExpression(Expression leftOperand, Expression rightOperand,
			int operator) {
		this.leftOperand = leftOperand;
		this.rightOperand = rightOperand;
		this.operator = operator;
	}

	private Expression leftOperand = null;
	private Expression rightOperand = null;

	private int operator = 0;

	public Expression getLeftOperand() {
		return leftOperand;
	}

	public int getOperator() {
		return operator;
	}

	public Expression getRightOperand() {
		return rightOperand;
	}

}
