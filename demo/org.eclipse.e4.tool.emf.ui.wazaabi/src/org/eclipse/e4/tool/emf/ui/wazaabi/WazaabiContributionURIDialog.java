package org.eclipse.e4.tool.emf.ui.wazaabi;

import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.resources.IProject;
import org.eclipse.e4.tool.emf.ui.wazaabi.IWazaabiURIContributionProvider.ContributionData;
import org.eclipse.e4.tool.emf.ui.wazaabi.IWazaabiURIContributionProvider.ContributionResultHandler;
import org.eclipse.e4.tools.emf.ui.common.IClassContributionProvider.Filter;
import org.eclipse.e4.tools.emf.ui.internal.Messages;
import org.eclipse.e4.ui.model.application.MApplicationElement;
import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.command.SetCommand;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

public class WazaabiContributionURIDialog extends TitleAreaDialog {
	private IProject project;
	private Image javaClassImage;
	private MApplicationElement contribution;
	private EditingDomain editingDomain;
	private TableViewer viewer;
	private EStructuralFeature feature;
	private Messages Messages;

	public WazaabiContributionURIDialog(Shell parentShell, IProject project,
			EditingDomain editingDomain, MApplicationElement contribution,
			EStructuralFeature feature, Messages Messages) {
		super(parentShell);
		this.project = project;
		this.contribution = contribution;
		this.editingDomain = editingDomain;
		this.feature = feature;
		this.Messages = Messages;
	}

	@Override
	protected boolean isResizable() {
		return true;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite) super.createDialogArea(parent);

		getShell().setText(Messages.ContributionClassDialog_ShellTitle);
		setTitle(Messages.ContributionClassDialog_DialogTitle);
		setMessage(Messages.ContributionClassDialog_DialogMessage);

		final Image titleImage = new Image(comp.getDisplay(), getClass()
				.getClassLoader().getResourceAsStream(
						"/icons/full/wizban/newclass_wiz.png")); //$NON-NLS-1$
		setTitleImage(titleImage);

		getShell().addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				javaClassImage.dispose();
				titleImage.dispose();
			}
		});

		javaClassImage = new Image(getShell().getDisplay(), getClass()
				.getClassLoader().getResourceAsStream(
						"/icons/full/obj16/class_obj.gif")); //$NON-NLS-1$

		Composite container = new Composite(comp, SWT.NONE);
		container.setLayoutData(new GridData(GridData.FILL_BOTH));
		container.setLayout(new GridLayout(2, false));

		Label l = new Label(container, SWT.NONE);
		l.setText("URI");

		final Text t = new Text(container, SWT.BORDER | SWT.SEARCH
				| SWT.ICON_SEARCH);
		t.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		t.setMessage(Messages.ContributionClassDialog_FilterText_Message);

		new Label(container, SWT.NONE);

		viewer = new TableViewer(container);
		GridData gd = new GridData(GridData.FILL_BOTH);
		viewer.getControl().setLayoutData(gd);
		viewer.setContentProvider(new ObservableListContentProvider());
		viewer.setLabelProvider(new StyledCellLabelProvider() {
			@Override
			public void update(ViewerCell cell) {
				ContributionData data = (ContributionData) cell.getElement();
				StyledString styledString = new StyledString(data.resourceName,
						null);

				if (data.bundleName != null) {
					styledString
							.append(" - " + data.bundleName, StyledString.DECORATIONS_STYLER); //$NON-NLS-1$
				}

//				if (data.sourceType != null) {
//					styledString.append(" - ", StyledString.DECORATIONS_STYLER); //$NON-NLS-1$
//					styledString.append(
//							data.sourceType + "", StyledString.COUNTER_STYLER); //$NON-NLS-1$
//				}
//
//				if (data.iconPath == null) {
//					cell.setImage(javaClassImage);
//				}

				cell.setText(styledString.getString());
				cell.setStyleRanges(styledString.getStyleRanges());
			}
		});
		viewer.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent event) {
				okPressed();
			}
		});

		final WritableList list = new WritableList();
		viewer.setInput(list);

		final WazaabiUriContributionCollector collector = getCollector();

		t.addModifyListener(new ModifyListener() {
			private ContributionResultHandlerImpl currentResultHandler;

			public void modifyText(ModifyEvent e) {
				if (currentResultHandler != null) {
					currentResultHandler.cancled = true;
				}
				list.clear();
				currentResultHandler = new ContributionResultHandlerImpl(list);
				IWazaabiURIContributionProvider.Filter filter = new IWazaabiURIContributionProvider.Filter(
						project, t.getText());
				collector.findContributions(filter, currentResultHandler);
				t.addKeyListener(new KeyAdapter() {
					public void keyPressed(KeyEvent e) {
						if (e.keyCode == SWT.ARROW_DOWN) {
							if (viewer.getTable().getItemCount() > 0) {
								viewer.getTable().setFocus();
								viewer.getTable().select(0);
							}
						}
					}
				});
				viewer.getTable().addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						super.keyPressed(e);
						if ((e.keyCode == SWT.ARROW_UP)
								&& (viewer.getTable().getSelectionIndex() == 0)) {
							t.setFocus();
						}
					}
				});
			}
		});

		return comp;
	}

	@Override
	protected void okPressed() {
		IStructuredSelection s = (IStructuredSelection) viewer.getSelection();
		if (!s.isEmpty()) {
			ContributionData cd = (ContributionData) s.getFirstElement();
			String uri = "bundleclass://" + cd.bundleName + "/" + cd.resourceName; //$NON-NLS-1$ //$NON-NLS-2$
			Command cmd = SetCommand.create(editingDomain, contribution,
					feature, uri);
			if (cmd.canExecute()) {
				editingDomain.getCommandStack().execute(cmd);
				super.okPressed();
			}
		}
	}

	private WazaabiUriContributionCollector getCollector() {
		Bundle bundle = FrameworkUtil
				.getBundle(WazaabiContributionURIDialog.class);
		BundleContext context = bundle.getBundleContext();
		ServiceReference ref = context
				.getServiceReference(WazaabiUriContributionCollector.class
						.getName());
		if (ref != null) {
			return (WazaabiUriContributionCollector) context.getService(ref);
		}
		return null;
	}

	private static class ContributionResultHandlerImpl implements
			ContributionResultHandler {
		private boolean cancled = false;
		private IObservableList list;

		public ContributionResultHandlerImpl(IObservableList list) {
			this.list = list;
		}

		public void result(ContributionData data) {
			if (!cancled) {
				list.add(data);
			}
		}

	}
}