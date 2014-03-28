package org.eclipse.wazaabi.ide.propertysheets.viewers.paths;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.viewers.DescriptorLabelColumn;

public class PathsTableViewer extends TableViewer {

	private final String PATH_FOR_INSERTION = "";//$NON-NLS-1$
	private final TargetChangeListener listener;

	public PathsTableViewer(Composite parent, TargetChangeListener listener) {
		this(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER,
				listener);
	}

	public PathsTableViewer(Composite parent, int style,
			TargetChangeListener listener) {
		this(new Table(parent, style), listener);
	}

	public PathsTableViewer(Table table, TargetChangeListener listener) {
		super(table);
		this.listener = listener;
		setContentProvider(new PathsContentProvider(PATH_FOR_INSERTION));
		createColumn();
	}

	protected void createColumn() {
		new DescriptorLabelColumn(this, listener, new EventDescriptorFactory(),
				PATH_FOR_INSERTION, new LabelPrinter() {

					@Override
					public String getLabel(EObject item) {
						if (item instanceof PropertyChangedEvent)
							return ((PropertyChangedEvent) item).getPath();
						return ((Event) item).getId();
					}

				}, "Events") {

			@Override
			protected EObject createRowWithoutDescriptor(Object element,
					Object value) {
				PropertyChangedEvent event = EDPEventsFactory.eINSTANCE
						.createPropertyChangedEvent();
				event.setPath((String) value);
				return event;
			}
		}; //$NON-NLS-1$
	}
}
