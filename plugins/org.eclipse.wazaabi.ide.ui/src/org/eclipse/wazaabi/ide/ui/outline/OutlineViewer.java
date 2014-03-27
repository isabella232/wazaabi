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

package org.eclipse.wazaabi.ide.ui.outline;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.engine.core.editparts.WidgetEditPart;
import org.eclipse.wazaabi.engine.core.gef.EditPart;
import org.eclipse.wazaabi.engine.core.views.WidgetView;
import org.eclipse.wazaabi.engine.edp.coderesolution.ICodeLocator;
import org.eclipse.wazaabi.engine.swt.commons.editparts.SWTRootEditPart;
import org.eclipse.wazaabi.engine.swt.commons.views.SWTWidgetView;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.ide.ui.editparts.AbstractComponentTreeEditPart;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Container;

public class OutlineViewer extends SWTControlViewer implements
		ISelectionChangedListener {

	private List<Control> selectedControls = new ArrayList<Control>();

	private PaintListener paintListener = new PaintListener() {

		public void paintControl(PaintEvent e) {
			int offset = 1;

			for (Control c : getSelectedControls()) {
				Point controlLocation = c.getLocation();
				e.gc.setLineStyle(SWT.LINE_SOLID);
				e.gc.setLineWidth(2);
				Color previousColor = e.gc.getForeground();
				e.gc.setForeground(e.display.getSystemColor(SWT.COLOR_RED));
				if (c == e.widget) // Composite
					e.gc.drawRectangle(offset, offset, c.getSize().x - 2
							* offset, c.getSize().y - 2 * offset);
				else if (c.getParent() == e.widget
						&& c.getClass() != Composite.class
						&& c.getClass() != Canvas.class)
					e.gc.drawRectangle(controlLocation.x - offset * 2,
							controlLocation.y - 2 * offset, c.getSize().x + 2
									* offset + 1, c.getSize().y + 2 * offset
									+ 1);
				e.gc.setForeground(previousColor);
			}
		}
	};

	public OutlineViewer(Composite parent) {
		super(parent);
		getRegistry().setServices(ICodeLocator.class,
				Arrays.asList(new Object[] { new IDECodeLocator() }), true);
	}

	public OutlineViewer(Composite parent, SWTRootEditPart rootEditPart) {
		super(parent, rootEditPart);
		getRegistry().setServices(ICodeLocator.class,
				Arrays.asList(new Object[] { new IDECodeLocator() }), true);
	}

	public void selectionChanged(SelectionChangedEvent event) {
		if (getControl() == null || getControl().isDisposed()
				|| event.getSelection() == null)
			return;
		selectedControls.clear();
		if (event.getSelection() instanceof StructuredSelection) {
			for (Object selected : ((StructuredSelection) event.getSelection())
					.toList()) {
				if (selected instanceof AbstractComponentTreeEditPart) {
					AbstractComponent outlineComponent = getOutlineComponent(((AbstractComponentTreeEditPart) selected)
							.getAbstractComponentModel());
					WidgetEditPart ep = (WidgetEditPart) getEditPartRegistry()
							.get(outlineComponent);
					if (ep != null) {
						WidgetView widgetView = ep.getWidgetView();
						if (widgetView instanceof SWTWidgetView
								&& ((SWTWidgetView) widgetView).getSWTWidget() instanceof Control)
							selectedControls
									.add((Control) ((SWTWidgetView) widgetView)
											.getSWTWidget());
					}

				}
			}
		}
		refreshSelection();
	}

	public List<Control> getSelectedControls() {
		return selectedControls;
	}

	public void refreshSelection() {
		if (getControl() != null && getControl().getParent() != null
				&& !getControl().getParent().isDisposed())
			getControl().getParent().redraw();
		if (getControl() instanceof Composite)
			deepRedraw((Composite) getControl());
	}

	protected void deepRedraw(Composite root) {
		root.redraw();
		for (Control child : root.getChildren())
			if (child instanceof Composite)
				deepRedraw((Composite) child);
	}

	protected AbstractComponent getOutlineComponent(
			AbstractComponent editorComponent) {
		if (editorComponent == null)
			return null;
		if (getRootEditPart().getContents() != null
				&& getRootEditPart().getContents().getModel() instanceof AbstractComponent) {
			AbstractComponent viewerRoot = (AbstractComponent) getRootEditPart()
					.getContents().getModel();
			if (editorComponent.eContainer() == null)
				return viewerRoot;

			if (!(viewerRoot instanceof Container))
				return null;
			List<Integer> indexes = new ArrayList<Integer>();
			AbstractComponent current = editorComponent;
			Container parent = null;
			while ((parent = (Container) current.eContainer()) != null) {
				indexes.add(parent.getChildren().indexOf(current));
				current = parent;
			}
			if (!indexes.isEmpty()) {
				current = viewerRoot;
				for (int i = indexes.size() - 1; i >= 0; i--) {
					int index = indexes.get(i);
					if (current instanceof Container
							&& ((Container) current).getChildren().size() > index)
						current = ((Container) current).getChildren()
								.get(index);
					else
						break;
				}
				return current;
			}
		}
		return null;
	}

	@Override
	public void doSetContents(EditPart editpart) {
		super.doSetContents(editpart);
		selectedControls.clear();
		if (getControl() instanceof Composite)
			addPaintListeners((Composite) getControl());
		if (getControl().getParent() != null) {
			getControl().getParent().addPaintListener(paintListener);
			getControl().addDisposeListener(new DisposeListener() {

				@Override
				public void widgetDisposed(DisposeEvent e) {
					if (getControl() != null
							&& getControl().getParent() != null
							&& !getControl().getParent().isDisposed())
						getControl().getParent().removePaintListener(
								paintListener);
				}
			});
		}
	}

	protected void addPaintListeners(Composite composite) {
		composite.addPaintListener(paintListener);
		for (Control child : composite.getChildren())
			if (child.getClass() == Composite.class
					|| child.getClass() == Canvas.class)
				addPaintListeners((Composite) child);
	}

}
