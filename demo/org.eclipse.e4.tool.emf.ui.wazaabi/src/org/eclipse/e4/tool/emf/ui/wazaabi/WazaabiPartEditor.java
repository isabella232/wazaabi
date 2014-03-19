package org.eclipse.e4.tool.emf.ui.wazaabi;

import javax.inject.Inject;

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.resources.IProject;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.tools.emf.ui.common.ContributionURIValidator;
import org.eclipse.e4.tools.emf.ui.common.IContributionClassCreator;
import org.eclipse.e4.tools.emf.ui.common.ImageTooltip;
import org.eclipse.e4.tools.emf.ui.common.Util;
import org.eclipse.e4.tools.emf.ui.internal.ResourceProvider;
import org.eclipse.e4.tools.emf.ui.internal.common.component.ControlFactory;
import org.eclipse.e4.tools.emf.ui.internal.common.component.PartEditor;
import org.eclipse.e4.tools.emf.ui.internal.common.component.ControlFactory.TextPasteHandler;
import org.eclipse.e4.tools.emf.ui.internal.common.component.dialogs.ContributionClassDialog;
import org.eclipse.e4.tools.emf.ui.internal.common.component.dialogs.PartIconDialogEditor;
import org.eclipse.e4.tools.emf.ui.internal.common.objectdata.ObjectViewer;
import org.eclipse.e4.tools.emf.ui.internal.common.uistructure.UIViewer;
import org.eclipse.e4.tools.services.IResourcePool;
import org.eclipse.e4.ui.model.application.MContribution;
import org.eclipse.e4.ui.model.application.impl.ApplicationPackageImpl;
import org.eclipse.e4.ui.model.application.ui.MUILabel;
import org.eclipse.e4.ui.model.application.ui.basic.MPart;
import org.eclipse.e4.ui.model.application.ui.basic.impl.BasicPackageImpl;
import org.eclipse.e4.ui.model.application.ui.impl.UiPackageImpl;
import org.eclipse.e4.ui.model.application.ui.menu.MMenuFactory;
import org.eclipse.e4.ui.model.application.ui.menu.MToolBar;
import org.eclipse.emf.common.command.Command;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.databinding.EMFDataBindingContext;
import org.eclipse.emf.databinding.edit.EMFEditProperties;
import org.eclipse.emf.edit.command.SetCommand;
import org.eclipse.jface.databinding.swt.IWidgetValueProperty;
import org.eclipse.jface.databinding.swt.WidgetProperties;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Text;

@SuppressWarnings("restriction")
public class WazaabiPartEditor extends PartEditor {
	private Button createRemoveToolBar;

	@Inject
	@Optional
	private IProject project;
	@Inject
	protected IResourcePool resourcePool;

	@Override
	public String getLabel(Object element) {
		return "Wazaabi Part";
	}

	@Override
	public String getDescription(Object element) {
		return "description";
	}

	@Override
	protected Composite createForm(Composite parent,
			EMFDataBindingContext context, IObservableValue master,
			boolean isImport) {
		CTabFolder folder = new CTabFolder(parent, SWT.BOTTOM);

		CTabItem item = new CTabItem(folder, SWT.NONE);
		item.setText(Messages.ModelTooling_Common_TabDefault);

		parent = createScrollableContainer(folder);
		item.setControl(parent.getParent());

		if (getEditor().isShowXMIId() || getEditor().isLiveModel()) {
			ControlFactory.createXMIId(parent, this);
		}

		IWidgetValueProperty textProp = WidgetProperties.text(SWT.Modify);

		if (isImport) {
			ControlFactory.createFindImport(parent, Messages, this, context);
			folder.setSelection(0);
			return folder;
		}

		ControlFactory
				.createTextField(
						parent,
						Messages.ModelTooling_Common_Id,
						master,
						context,
						textProp,
						EMFEditProperties
								.value(getEditingDomain(),
										ApplicationPackageImpl.Literals.APPLICATION_ELEMENT__ELEMENT_ID));
		ControlFactory.createTextField(parent, Messages.PartEditor_LabelLabel,
				master, context, textProp, EMFEditProperties.value(
						getEditingDomain(),
						UiPackageImpl.Literals.UI_LABEL__LABEL));
		ControlFactory
				.createTextField(
						parent,
						Messages.ModelTooling_UIElement_AccessibilityPhrase,
						master,
						context,
						textProp,
						EMFEditProperties
								.value(getEditingDomain(),
										UiPackageImpl.Literals.UI_ELEMENT__ACCESSIBILITY_PHRASE));
		ControlFactory.createTextField(parent, Messages.PartEditor_Tooltip,
				master, context, textProp, EMFEditProperties.value(
						getEditingDomain(),
						UiPackageImpl.Literals.UI_LABEL__TOOLTIP));

		// ------------------------------------------------------------
		{
			Label l = new Label(parent, SWT.NONE);
			l.setText(Messages.PartEditor_IconURI);
			l.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));

			final Text t = new Text(parent, SWT.BORDER);
			TextPasteHandler.createFor(t);
			t.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			context.bindValue(
					textProp.observeDelayed(200, t),
					EMFEditProperties.value(getEditingDomain(),
							UiPackageImpl.Literals.UI_LABEL__ICON_URI)
							.observeDetail(master));

			new ImageTooltip(t, Messages) {

				@Override
				protected URI getImageURI() {
					MUILabel part = (MUILabel) getMaster().getValue();
					String uri = part.getIconURI();
					if (uri == null || uri.trim().length() == 0) {
						return null;
					}
					return URI.createURI(part.getIconURI());
				}
			};

			final Button b = new Button(parent, SWT.PUSH | SWT.FLAT);
			b.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false,
					false));
			b.setImage(createImage(ResourceProvider.IMG_Obj16_zoom));
			b.setText(Messages.ModelTooling_Common_FindEllipsis);
			b.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					PartIconDialogEditor dialog = new PartIconDialogEditor(b
							.getShell(), project, getEditingDomain(),
							(MPart) getMaster().getValue(), Messages);
					dialog.open();
				}
			});
		}

		// ------------------------------------------------------------
		final Link lnk = new Link(parent, SWT.NONE);
		{
			final IContributionClassCreator c = getEditor()
					.getContributionCreator(BasicPackageImpl.Literals.PART);
			if (project != null && c != null) {

				lnk.setText("<A>" + "Wazaabi resource URI" + "</A>"); //$NON-NLS-1$//$NON-NLS-2$
				lnk.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));
				lnk.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						c.createOpen((MContribution) getMaster().getValue(),
								getEditingDomain(), project, lnk.getShell());
					}
				});
			} else {
				Label l = new Label(parent, SWT.NONE);
				l.setText("Wazaabi resource URI");
				l.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));
			}

			Text t = new Text(parent, SWT.BORDER);
			TextPasteHandler.createFor(t);
			t.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			t.addModifyListener(new ModifyListener() {

				public void modifyText(ModifyEvent e) {
					lnk.setToolTipText(((Text) (e.getSource())).getText());
				}
			});
			Binding binding = context
					.bindValue(
							textProp.observeDelayed(200, t),
							EMFEditProperties
									.value(getEditingDomain(),
											ApplicationPackageImpl.Literals.CONTRIBUTION__CONTRIBUTION_URI)
									.observeDetail(master),
							new UpdateValueStrategy()
									.setAfterConvertValidator(new ContributionURIValidator()),
							new UpdateValueStrategy());
			Util.addDecoration(t, binding);

			final Button b = new Button(parent, SWT.PUSH | SWT.FLAT);
			b.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, false,
					false));
			b.setImage(createImage(ResourceProvider.IMG_Obj16_zoom));
			b.setText("URI");
			b.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					WazaabiContributionURIDialog dialog = new WazaabiContributionURIDialog(
							b.getShell(),
							project,
							getEditingDomain(),
							(MContribution) getMaster().getValue(),
							ApplicationPackageImpl.Literals.CONTRIBUTION__CONTRIBUTION_URI,
							Messages);
					dialog.open();
				}
			});
		}

		// ------------------------------------------------------------
		{
			Label l = new Label(parent, SWT.NONE);
			l.setText(Messages.PartEditor_ToolBar);
			l.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));

			createRemoveToolBar = new Button(parent, SWT.CHECK);
			createRemoveToolBar.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					MPart window = (MPart) getMaster().getValue();
					if (window.getToolbar() == null) {
						addToolBar();
					} else {
						removeToolBar();
					}
				}
			});
			createRemoveToolBar.setLayoutData(new GridData(GridData.BEGINNING,
					GridData.CENTER, false, false, 2, 1));
		}

		ControlFactory.createTextField(parent,
				Messages.PartEditor_ContainerData, master, context, textProp,
				EMFEditProperties.value(getEditingDomain(),
						UiPackageImpl.Literals.UI_ELEMENT__CONTAINER_DATA));

		createSubformElements(parent, context, master);

		ControlFactory.createCheckBox(parent, Messages.PartEditor_Closeable,
				Messages.PartEditor_Closeable_Tooltip, getMaster(), context,
				WidgetProperties.selection(), EMFEditProperties.value(
						getEditingDomain(),
						BasicPackageImpl.Literals.PART__CLOSEABLE));

		// ------------------------------------------------------------
		ControlFactory.createCheckBox(parent,
				Messages.ModelTooling_UIElement_ToBeRendered, getMaster(),
				context, WidgetProperties.selection(), EMFEditProperties.value(
						getEditingDomain(),
						UiPackageImpl.Literals.UI_ELEMENT__TO_BE_RENDERED));
		ControlFactory.createCheckBox(parent,
				Messages.ModelTooling_UIElement_Visible, getMaster(), context,
				WidgetProperties.selection(), EMFEditProperties.value(
						getEditingDomain(),
						UiPackageImpl.Literals.UI_ELEMENT__VISIBLE));

		ControlFactory.createBindingContextWiget(parent, Messages, this,
				Messages.PartEditor_BindingContexts);
		ControlFactory
				.createMapProperties(
						parent,
						Messages,
						this,
						Messages.ModelTooling_Contribution_PersistedState,
						ApplicationPackageImpl.Literals.APPLICATION_ELEMENT__PERSISTED_STATE,
						VERTICAL_LIST_WIDGET_INDENT);
		ControlFactory.createMapProperties(parent, Messages, this,
				Messages.ModelTooling_Context_Properties,
				UiPackageImpl.Literals.CONTEXT__PROPERTIES,
				VERTICAL_LIST_WIDGET_INDENT);

		item = new CTabItem(folder, SWT.NONE);
		item.setText(Messages.ModelTooling_Common_TabSupplementary);

		parent = createScrollableContainer(folder);
		item.setControl(parent.getParent());

		ControlFactory.createStringListWidget(parent, Messages, this,
				Messages.ModelTooling_Context_Variables,
				Messages.ModelTooling_Context_Variables_Tooltip,
				UiPackageImpl.Literals.CONTEXT__VARIABLES,
				VERTICAL_LIST_WIDGET_INDENT);
		ControlFactory.createStringListWidget(parent, Messages, this,
				Messages.CategoryEditor_Tags,
				ApplicationPackageImpl.Literals.APPLICATION_ELEMENT__TAGS,
				VERTICAL_LIST_WIDGET_INDENT);

		if (project == null) {
			createInstanceInspection(folder);
			createUITreeInspection(folder);
		}

		folder.setSelection(0);

		return folder;
	}

	private void addToolBar() {
		MToolBar menu = MMenuFactory.INSTANCE.createToolBar();
		setElementId(menu);

		Command cmd = SetCommand.create(getEditingDomain(), getMaster()
				.getValue(), BasicPackageImpl.Literals.PART__TOOLBAR, menu);
		if (cmd.canExecute()) {
			getEditingDomain().getCommandStack().execute(cmd);
		}
	}

	private void removeToolBar() {
		Command cmd = SetCommand.create(getEditingDomain(), getMaster()
				.getValue(), BasicPackageImpl.Literals.PART__TOOLBAR, null);
		if (cmd.canExecute()) {
			getEditingDomain().getCommandStack().execute(cmd);
		}
	}

	private void createInstanceInspection(CTabFolder folder) {
		CTabItem item = new CTabItem(folder, SWT.NONE);
		item.setText(Messages.ModelTooling_Common_RuntimeContributionInstance);
		Composite container = new Composite(folder, SWT.NONE);
		container.setLayout(new GridLayout());
		item.setControl(container);

		ObjectViewer objectViewer = new ObjectViewer();
		TreeViewer viewer = objectViewer.createViewer(container,
				ApplicationPackageImpl.Literals.CONTRIBUTION__OBJECT,
				getMaster(), resourcePool, Messages);
		viewer.getControl().setLayoutData(new GridData(GridData.FILL_BOTH));
	}

	private void createUITreeInspection(CTabFolder folder) {
		CTabItem item = new CTabItem(folder, SWT.NONE);
		item.setText(Messages.ModelTooling_Common_RuntimeWidgetTree);
		Composite container = new Composite(folder, SWT.NONE);
		container.setLayout(new GridLayout());
		item.setControl(container);

		UIViewer objectViewer = new UIViewer();
		TreeViewer viewer = objectViewer.createViewer(container,
				UiPackageImpl.Literals.UI_ELEMENT__WIDGET, getMaster(),
				resourcePool, Messages);
		viewer.getControl().setLayoutData(new GridData(GridData.FILL_BOTH));
	}
}
