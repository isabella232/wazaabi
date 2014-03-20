package eclipsecon2014;

import java.io.IOException;

import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.rap.rwt.application.AbstractEntryPoint;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.demo.contacts.database.DataService;
import org.eclipse.wazaabi.engine.rap.viewers.RapControlViewer;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class Standalone extends AbstractEntryPoint {

	@Override
	protected void createContents(Composite parent) {

		// create the viewer
		RapControlViewer viewer = new RapControlViewer(parent);

		SWTHelper.init(viewer);
		URNJavaLocatorHelper.init(viewer);
		LocationPathsHelper.init(viewer);

		DataService service = new DataService();
		AbstractComponent root = readUI("ContactForm.ui");
		root.set("input", service.getDatabase().getContacts().get(1));

		viewer.setContents(root);

	}

	protected AbstractComponent readUI(String fileName) {
		SWTStylesPackage.eINSTANCE.eClass();
		XMIResource res = new XMIResourceImpl();
		try {
			res.load(getClass().getClassLoader().getResourceAsStream(fileName),
					null);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}

		if (!res.getContents().isEmpty()
				&& res.getContents().get(0) instanceof AbstractComponent)
			return (AbstractComponent) res.getContents().get(0);
		return null;
	}
}
