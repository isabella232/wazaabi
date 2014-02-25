/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.editinghelpers;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColorCellEditor;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.table.TargetChangeListener;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;

public class ColorEditingHelper extends AbstractEditingHelper {
	private static List<EStructuralFeature> features = new ArrayList<EStructuralFeature>(
			Arrays.asList(new EStructuralFeature[] {
					CoreStylesPackage.Literals.COLOR_RULE__RED,
					CoreStylesPackage.Literals.COLOR_RULE__GREEN,
					CoreStylesPackage.Literals.COLOR_RULE__BLUE }));

	private static List<Integer> positions = new ArrayList<Integer>(
			Arrays.asList(new Integer[] { -1, -1, -1 }));

	@Override
	public CellEditor getCellEditor(Control control, Object element) {
		return new ColorCellEditor((Composite) control);
	}

	@Override
	public Object getValue(Object element) {
		return new RGB(((ColorRule) element).getRed(),
				((ColorRule) element).getGreen(),
				((ColorRule) element).getBlue());
	}

	@Override
	public void setValue(Object element, Object value,
			TargetChangeListener listener) {
		List<Object> oldValues = new ArrayList<Object>(
				Arrays.asList(new Object[] { ((ColorRule) element).getRed(),
						((ColorRule) element).getGreen(),
						((ColorRule) element).getBlue() }));

		List<Object> newValues = new ArrayList<Object>(
				Arrays.asList(new Object[] { ((RGB) value).red,
						((RGB) value).green, ((RGB) value).blue }));
		listener.targetMultipleModified((EObject) element, features, positions,
				oldValues, newValues);
	}

}
