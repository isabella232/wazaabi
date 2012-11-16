package org.eclipse.wazaabi.engine.swt.addressbook;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class AddressBookView extends ViewPart {

	SWTControlViewer viewer = null;
	
	Container rootContainer = null;
	
	public AddressBookView() {
		rootContainer = CoreWidgetsFactory.eINSTANCE.createContainer();
		FillLayoutRule fill = SWTStylesFactory.eINSTANCE.createFillLayoutRule();
		fill.setPropertyName("layout");
		rootContainer.getStyleRules().add(fill);
		GridLayoutRule grid = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
		grid.setPropertyName("layout");
		grid.setNumColumns(2);
		//	grid.setHorizontalSpacing(500);
		//rootContainer.getStyleRules().add(grid);
		
		GridDataRule gridData = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gridData.setPropertyName("layout-data");
		gridData.setGrabExcessHorizontalSpace(true);
		gridData.setGrabExcessVerticalSpace(true);
		gridData.setHorizontalAlignement(GridDataAlignment.FILL);
		gridData.setVerticalAlignement(GridDataAlignment.FILL);
		gridData.setVerticalSpan(100);
		rootContainer.getStyleRules().add(gridData);
		
		Container masterContainer = AddressBookUIHelper.createMasterUI();
		Container detailContainer = AddressBookUIHelper.createDetailUI();
		
		rootContainer.getChildren().add(masterContainer);
		rootContainer.getChildren().add(detailContainer);
		
		EObject addressBookContent = (EObject) AddressBookHelper.createAddressBook();
		rootContainer.set("datamodel", addressBookContent);
	}
	
	@Override
	public void createPartControl(Composite parent) {
		viewer = new SWTControlViewer(parent);
		viewer.setContents(rootContainer);
		viewer.getControl();
	}

	@Override
	public void setFocus() {
		if (viewer != null) {
			viewer.getControl().setFocus();
		}
	}

}
