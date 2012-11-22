package org.eclipse.wazaabi.engine.swt.addressbook;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.wazaabi.engine.core.editparts.AbstractComponentEditPart;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.Label;
import org.eclipse.wazaabi.mm.swt.styles.GridDataAlignment;
import org.eclipse.wazaabi.mm.swt.styles.GridDataRule;
import org.eclipse.wazaabi.mm.swt.styles.GridLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;

public class AddressBookView extends ViewPart {

	SWTControlViewer viewer = null;
	
	Container fullContainer = null;
	
	public AddressBookView() {
		fullContainer = CoreWidgetsFactory.eINSTANCE.createContainer();
		GridLayoutRule gridL = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
		gridL.setPropertyName("layout");
		fullContainer.getStyleRules().add(gridL);
				
		Label titleLabel = CoreWidgetsFactory.eINSTANCE.createLabel();
		titleLabel.setText("Master-Detail Example");
		
		GridDataRule titleData = SWTStylesFactory.eINSTANCE.createGridDataRule();
		titleData.setPropertyName("layout-data");
		titleLabel.getStyleRules().add(titleData);
		
		FontRule titleFont = CoreStylesFactory.eINSTANCE.createFontRule();
		titleFont.setBold(true);
		titleFont.setPropertyName(AbstractComponentEditPart.FONT_PROPERTY_NAME);
		titleFont.setHeight(12);
		titleLabel.getStyleRules().add(titleFont);
		
		fullContainer.getChildren().add(titleLabel);
		
		Container rootContainer = CoreWidgetsFactory.eINSTANCE.createContainer();
		fullContainer.getChildren().add(rootContainer);
		rootContainer.setId("rootContainer");

		GridLayoutRule rootGrid = SWTStylesFactory.eINSTANCE.createGridLayoutRule();
		rootGrid.setPropertyName("layout");
		rootGrid.setNumColumns(2);
		rootGrid.setMakeColumnsEqualWidth(true);
		rootContainer.getStyleRules().add(rootGrid);
		
		GridDataRule gridData = SWTStylesFactory.eINSTANCE.createGridDataRule();
		gridData.setPropertyName("layout-data");
		gridData.setGrabExcessHorizontalSpace(true);
		gridData.setGrabExcessVerticalSpace(true);
		gridData.setHorizontalAlignement(GridDataAlignment.FILL);
		gridData.setVerticalAlignement(GridDataAlignment.FILL);
		rootContainer.getStyleRules().add(gridData);
		
//		ColorRule color = CoreStylesFactory.eINSTANCE.createColorRule();
//		color.setPropertyName("background-color");
//		color.setBlue(255);
//		rootContainer.getStyleRules().add(color);
		
		Container masterContainer = AddressBookUIHelper.createMasterUI();
		Container detailContainer = AddressBookUIHelper.createEmptyDetailUI();
		
		rootContainer.getChildren().add(masterContainer);
		rootContainer.getChildren().add(detailContainer);
		
		EObject addressBookContent = (EObject) AddressBookHelper.createAddressBook();
		rootContainer.set("datamodel", addressBookContent);
	}
	
	@Override
	public void createPartControl(Composite parent) {
		viewer = new SWTControlViewer(parent);
		viewer.setContents(fullContainer);
		viewer.getControl();
		fullContainer.set("viewer", viewer);
	}

	@Override
	public void setFocus() {
		if (viewer != null) {
			viewer.getControl().setFocus();
		}
	}

}
