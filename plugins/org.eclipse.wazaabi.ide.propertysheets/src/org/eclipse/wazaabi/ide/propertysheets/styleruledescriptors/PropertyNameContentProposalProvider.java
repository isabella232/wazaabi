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

package org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.fieldassist.ContentProposal;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public class PropertyNameContentProposalProvider implements
        IContentProposalProvider {

    private final StyledElement styledElement;

    public PropertyNameContentProposalProvider(StyledElement styledElement) {
        this.styledElement = styledElement;
    }

    public IContentProposal[] getProposals(String contents, int position) {
        Set<StyleRuleDescriptor> descriptors = new StyleRuleDescriptorFactory()
                .getDescriptors(styledElement);
        List<IContentProposal> contentProposals = new ArrayList<IContentProposal>();
        for (StyleRuleDescriptor descriptor : descriptors)
            if (descriptor.getPropertyName().startsWith(contents)) {
                ContentProposal contentProposal = new ContentProposal(
                        descriptor.getPropertyName());
                contentProposals.add(contentProposal);
            }
        return contentProposals.toArray(new IContentProposal[] {});
    }

}