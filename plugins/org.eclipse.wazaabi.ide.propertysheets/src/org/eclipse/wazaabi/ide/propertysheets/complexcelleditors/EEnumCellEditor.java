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

package org.eclipse.wazaabi.ide.propertysheets.complexcelleditors;

import java.util.List;

import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.mm.core.Direction;

public class EEnumCellEditor extends CellEditor {

	private EEnumLiteral selection;
	private final List<EEnumLiteral> eEnumLiterals;

	private Composite container;

	private static final int defaultStyle = SWT.NONE;

	public EEnumCellEditor(Composite parent, EEnum eEnum) {
		this(parent, eEnum.getELiterals());
	}

	public EEnumCellEditor(Composite parent, List<EEnumLiteral> eEnumLiterals) {
		this.eEnumLiterals = eEnumLiterals;
		setStyle(defaultStyle);
		create(parent);
	}

	void applyEditorValueAndDeactivate() {
		Object newValue = doGetValue();
		markDirty();
		setValueValid(isCorrect(newValue));
		fireApplyEditorValue();
		deactivate();
	}

	protected Control createControl(Composite parent) {
		container = new Composite(parent, SWT.NONE);

		container.setLayout(new RowLayout());

		for (EEnumLiteral eEnumLiteral : getEEnumLiterals())
			createRadio(parent, eEnumLiteral);
		return container;
	}

	protected Button createRadio(Composite parent,
			final EEnumLiteral eEnumLiteral) {
		Button radio = new Button(container, SWT.RADIO);
		radio.setFont(parent.getFont());
		radio.setText(eEnumLiteral.getName()); // Should be translated or
												// fetched from descriptor
		radio.setData(eEnumLiteral);

		radio.addSelectionListener(new SelectionAdapter() {
			public void widgetDefaultSelected(SelectionEvent event) {
				applyEditorValueAndDeactivate();
			}

			public void widgetSelected(SelectionEvent event) {
				setSelection(eEnumLiteral);
			}
		});

		radio.addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				keyReleaseOccured(e);
			}
		});

		radio.addTraverseListener(new TraverseListener() {
			public void keyTraversed(TraverseEvent e) {
				if (e.detail == SWT.TRAVERSE_ESCAPE
						|| e.detail == SWT.TRAVERSE_RETURN) {
					e.doit = false;
				}
			}
		});
//
//		radio.addFocusListener(new FocusAdapter() {
//			public void focusLost(FocusEvent e) {
//				EEnumCellEditor.this.focusLost();
//			}
//		});
		return radio;
	}

	protected Object doGetValue() {
		return getSelection();
	}

	protected void doSetFocus() {
		container.setFocus();
	}

	protected void doSetValue(Object value) {
		assert container != null && (value instanceof Direction);
		for (Control child : container.getChildren())
			if (child instanceof Button
					&& (child.getStyle() & SWT.RADIO) == SWT.RADIO)
				((Button) child).setSelection(((EEnumLiteral) child.getData())
						.getInstance() == value);
	}

	protected void focusLost() {
		if (isActivated())
			applyEditorValueAndDeactivate();
	}

	public List<EEnumLiteral> getEEnumLiterals() {
		return eEnumLiterals;
	}

	public LayoutData getLayoutData() {
		LayoutData layoutData = super.getLayoutData();
		if ((container == null) || container.isDisposed()) {
			layoutData.minimumWidth = 60;
		} else {
			// make the comboBox 10 characters wide
			GC gc = new GC(container);
			layoutData.minimumWidth = (gc.getFontMetrics()
					.getAverageCharWidth() * 10) + 10;
			gc.dispose();
		}
		return layoutData;
	}

	public EEnumLiteral getSelection() {
		return selection;
	}

	protected void keyReleaseOccured(KeyEvent keyEvent) {
		if (keyEvent.character == '\u001b') { // Escape character
			fireCancelEditor();
		} else if (keyEvent.character == '\r') { // CR
			applyEditorValueAndDeactivate();
		}
	}

	public void setSelection(EEnumLiteral selection) {
		this.selection = selection;
	}
}
