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

package org.eclipse.wazaabi.engine.swt.views;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.MenuComponentEditPart;
import org.eclipse.wazaabi.engine.core.views.MenuComponentView;
import org.eclipse.wazaabi.engine.swt.editparts.stylerules.managers.ImageRuleManager;
import org.eclipse.wazaabi.engine.swt.viewers.AbstractSWTViewer;
import org.eclipse.wazaabi.mm.core.Direction;
import org.eclipse.wazaabi.mm.core.styles.DirectionRule;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.widgets.MenuComponent;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;


public class SWTMenuComponentView extends SWTWidgetView implements
		MenuComponentView {
	
	private Image image = null;
	
	private boolean valid = false;

	
	private SelectionListener selectionListener = new SelectionListener() {
		public void widgetSelected(SelectionEvent e) {
//			int newValue = ((org.eclipse.swt.widgets.Scale) e.widget)
//					.getSelection();
//			if (((Scale) getHost().getModel()).getValue() != newValue)
//				((Scale) getHost().getModel()).setValue(newValue);
			System.out.println(e.getSource() + " selected");
		}

		public void widgetDefaultSelected(SelectionEvent e) {
		}
	};

	protected SelectionListener getSelectionListener() {
		return this.selectionListener;
	}

	
	public void setText(String text) {
		if(getSWTWidget() instanceof org.eclipse.swt.widgets.MenuItem)
			((org.eclipse.swt.widgets.MenuItem) getSWTWidget()).setText(text);		
	}

	
	public String getText() {
		if(getSWTWidget() instanceof org.eclipse.swt.widgets.MenuItem)
			return ((org.eclipse.swt.widgets.MenuItem) getSWTWidget()).getText();
		return null;
	}
	
	protected void setImage(ImageRule rule) {
		if (getSWTWidget() instanceof org.eclipse.swt.widgets.MenuItem) {

			if (rule == null)
				if (image == null)
					return;
				else {
					System.out.println("disposing image from "
							+ System.identityHashCode(this));
					image.dispose();
					image = null;
				}
			else {
				Image newImage = ImageRuleManager
						.convertToPlatformSpecificObject(this, rule);
				if (image != null) {
					if (newImage != null
							&& image.getImageData().equals(
									newImage.getImageData()))
						return;
					System.out.println("disposing image from "
							+ System.identityHashCode(this));
					image.dispose();
				}
				image = newImage;
			}
			((org.eclipse.swt.widgets.MenuItem) getSWTWidget()).setImage(image);
			//getSWTControl().update();
			System.out.println("setImage " + image);
			revalidate();
		}
	}
	
	public void setEnabled(boolean enabled){
		if(getSWTWidget() instanceof org.eclipse.swt.widgets.MenuItem)
			((org.eclipse.swt.widgets.MenuItem) getSWTWidget()).setEnabled(enabled);
		
	}
	
	public boolean isEnabled(){
		if(getSWTWidget() instanceof org.eclipse.swt.widgets.MenuItem)
			return ((org.eclipse.swt.widgets.MenuItem)getSWTWidget()).isEnabled();
		return true;
	}
	
	@Override
	public void updateStyleRule(StyleRule rule) {
		if (rule == null)
			return;
		if (MenuComponentEditPart.IMAGE_PROPERTY_NAME.equals(rule
				.getPropertyName()))
			if (rule instanceof ImageRule)
				setImage((ImageRule) rule);
			else
				setImage(null);
	}
	@Override
	public boolean needReCreateWidgetView(StyleRule styleRule, org.eclipse.swt.widgets.Widget widget){
		if(styleRule instanceof StringRule
				&& MenuComponentEditPart.TYPE_PROPERTY_NAME.equals(styleRule.getPropertyName())){
				return true;
		}
		else
			return super.needReCreateWidgetView(styleRule, widget);
	}

	@Override
	protected int computeSWTCreationStyle(StyleRule rule){
		if (rule instanceof DirectionRule){
			if(((DirectionRule) rule).getValue() == Direction.LEFT_TO_RIGHT)
				return SWT.LEFT_TO_RIGHT;
			if(((DirectionRule) rule).getValue() == Direction.RIGHT_TO_LEFT)
				return SWT.RIGHT_TO_LEFT;
		}
		return super.computeSWTCreationStyle(rule);
	}
	
	@Override
	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		int style = computeSWTCreationStyle(getHost());
		String type = ((StringRule) ((StyledElement) getHost().getModel())
				.getFirstStyleRule(MenuComponentEditPart.TYPE_PROPERTY_NAME,
						null)).getValue();

		if (type.equals("top-bar")) {
			final org.eclipse.swt.widgets.Menu menu = new org.eclipse.swt.widgets.Menu(
					(Shell) parent, style|SWT.BAR);
			((Shell)parent).setMenuBar(menu);
			return menu;
		} else if (type.equals("top-context")) {
			final org.eclipse.swt.widgets.Menu menu = new org.eclipse.swt.widgets.Menu(
					(Shell) parent, style|SWT.POP_UP);
			((Shell)parent).setMenu(menu);
			return menu;
		} else if (type.equals("check")) {
			final org.eclipse.swt.widgets.MenuItem menu = new org.eclipse.swt.widgets.MenuItem(
					(Menu) parent, style | SWT.CHECK);
			if (getSelectionListener() != null)
				menu.addSelectionListener(getSelectionListener());
			return menu;
		} else if (type.equals("separator")) {
			final org.eclipse.swt.widgets.MenuItem menu = new org.eclipse.swt.widgets.MenuItem(
					(Menu) parent, style | SWT.SEPARATOR);
			return menu;
		} else if (type.equals("radio")) {
			final org.eclipse.swt.widgets.MenuItem menu = new org.eclipse.swt.widgets.MenuItem(
					(Menu) parent, style | SWT.RADIO);
			if (getSelectionListener() != null)
				menu.addSelectionListener(getSelectionListener());
			return menu; 
		
		} else if (type.equals("push")) {
			final org.eclipse.swt.widgets.MenuItem menu = new org.eclipse.swt.widgets.MenuItem(
					(Menu) parent, style | SWT.PUSH);
			if (getSelectionListener() != null)
				menu.addSelectionListener(getSelectionListener());
			return menu; 
		} else if (type.equals("submenu")) {
			final org.eclipse.swt.widgets.MenuItem item = new org.eclipse.swt.widgets.MenuItem((Menu) parent,SWT.CASCADE|style);
			final org.eclipse.swt.widgets.Menu submenu = new org.eclipse.swt.widgets.Menu((Menu)parent);
			item.setText(((MenuComponent)(getHost().getModel())).getText());
			item.setMenu(submenu);
			return submenu;
			
		} else 
			return null;
		
	}
	
	// FIXME : rename this ugly named method !!
	protected void widgetDisposed() {
		super.widgetDisposed();
		if (getSWTWidget() != null && !getSWTWidget().isDisposed()
				&& getSelectionListener() != null)
			if (getSWTWidget() instanceof org.eclipse.swt.widgets.MenuItem)
			((org.eclipse.swt.widgets.MenuItem) getSWTWidget())
					.removeSelectionListener(getSelectionListener());
		
		if (image != null && !image.isDisposed()) {
			System.out.println("disposing image from "
					+ System.identityHashCode(this));
			image.dispose();
		}
	}

	@Override
	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.MENU_COMPONENT;
	}
	
	
//======================================================================================
// The following code comes from SWTControlView => might might be moved up to SWTWidget 
//======================================================================================
	public void validate() {
		// System.out.print("++++ validating "
		// + getSWTWidget().getClass().getSimpleName() + "[");
		//
		// if (getSWTWidget() instanceof Button)
		// System.out.print(((Button) getSWTWidget()).getText());
		// else if (getSWTWidget() instanceof Text)
		// System.out.print(((Text) getSWTWidget()).getText());
		// System.out.println("], (" + System.identityHashCode(this) + ")");

		if (isValid())
			return;
		setValid(true);

		fireWidgetViewValidated();
	}
	
	public void setValid(boolean value) {
		valid = value;
	}

	protected boolean isValid() {
		return valid;
	}
	
	public void addNotify() {
		assert getHost() != null;
		if (getSWTWidget() != null && !getSWTWidget().isDisposed())
			getSWTWidget().setData(WAZAABI_HOST_KEY, getHost());
	}
	
	public void removeNotify() {
	}
	
	public void revalidate() {
		invalidate();
		if (getParent() == null || isValidationRoot())
			getUpdateManager().addInvalidFigure(this);
		else
			getParent().revalidate();
	}
	
	public UpdateManager getUpdateManager() {
		return ((AbstractSWTViewer) getHost().getViewer()).getUpdateManager();
	}
	
	protected boolean isValidationRoot() {
		return false;
	}
	public void invalidate() {
		// if (layoutManager != null)
		// layoutManager.invalidate();
		setValid(false);
	}


	public void processPostControlCreation() {
		// TODO Auto-generated method stub
		
	}

}
