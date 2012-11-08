/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)GetPartnerLinkTypeVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.impl;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

/**
 * Visitor that collects all partner link types or finds one specific
 * partner link type from a WSDL and its imported ones.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
class GetPartnerLinkTypeVisitor extends AbstractVisitor {

    protected QName _pltName;
    protected PartnerLinkType _partnerLinkType;
    protected Set<PartnerLinkType> _partnerLinkTypes =
        new LinkedHashSet<PartnerLinkType>();
    
    /**
     * Default constructor that enables collecting all partner link types.
     */
    public GetPartnerLinkTypeVisitor() {
    }
    
    public GetPartnerLinkTypeVisitor(QName pltName) {
        _pltName = pltName;
    }
    
    @Override
    public boolean visit(DefinitionEx parentDef, DefinitionEx currentDef) {
        if (_pltName != null) {
            PartnerLinkType plt = currentDef.getPartnerLinkType(_pltName);
            if (plt != null) {
                _partnerLinkType = plt;
                return false;
            }
            return true;
        }
        _partnerLinkTypes.addAll(currentDef.getPartnerLinkTypes());
        return true;
    }
    
    public PartnerLinkType getPartnerLinkType() {
        return _partnerLinkType;
    }
    
    public Collection<PartnerLinkType> getPartnerLinkTypes() {
        return _partnerLinkTypes;
    }
}
