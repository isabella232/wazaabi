package eclipsecon2014;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.demo.contacts.database.DataService;
import org.eclipse.wazaabi.swt.starterkit.Wazaabi;

public class Standalone {

	public static void main(String args[]) {
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(300, 300);

		DataService service = new DataService();
		Wazaabi.createUI(mainShell, "urn:java:ContactForm.ui", service
				.getDatabase().getContacts().get(1));

		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

}
