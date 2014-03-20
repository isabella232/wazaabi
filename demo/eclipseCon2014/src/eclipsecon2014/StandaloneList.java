package eclipsecon2014;

import java.io.IOException;

import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.demo.contacts.database.DataService;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;
import org.eclipse.wazaabi.swt.starterkit.Wazaabi;

public class StandaloneList {

	public static void main(String args[]) {
		new StandaloneList().createContents();
	}

	protected void createContents() {
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		AbstractComponent root = readUI("List.ui");
		DataService service = new DataService();

		((Collection) ((Container) root).getChildren().get(0)).setInput(service
				.getDatabase());

		Wazaabi.createUI(mainShell, root, null);

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();

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
