/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.viewers;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.fieldassist.ContentProposal;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;

public class LabelContentProposalProvider implements IContentProposalProvider {

	private final EObject input;
	private final AbstractDescriptorFactory descriptorFactory;

	public LabelContentProposalProvider(EObject input,
			AbstractDescriptorFactory descriptorFactory) {
		this.input = input;
		this.descriptorFactory = descriptorFactory;
	}

	public IContentProposal[] getProposals(String contents, int position) {
		Set<AbstractDescriptor> descriptors = descriptorFactory
				.getDescriptors(input.eClass());
		if (descriptors == null)
			return new IContentProposal[] {};
		List<IContentProposal> contentProposals = new ArrayList<IContentProposal>();
		for (AbstractDescriptor descriptor : descriptors)
			if (descriptor.getId().startsWith(contents)) {
				ContentProposal contentProposal = new ContentProposal(
						descriptor.getId());
				contentProposals.add(contentProposal);
			}
		return contentProposals.toArray(new IContentProposal[] {});
	}

}