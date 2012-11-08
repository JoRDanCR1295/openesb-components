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
 * @(#)PopulatedExtensionRegistryEx.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.impl;

import javax.wsdl.Definition;

import com.ibm.wsdl.extensions.PopulatedExtensionRegistry;
import com.sun.wsdl4j.ext.bpel.impl.BPELExtConstants;
import com.sun.wsdl4j.ext.bpel.impl.BPELExtDeserializer;
import com.sun.wsdl4j.ext.bpel.impl.BPELExtSerializer;
import com.sun.wsdl4j.ext.bpel.impl.MessagePropertyAliasImpl;
import com.sun.wsdl4j.ext.bpel.impl.MessagePropertyImpl;
import com.sun.wsdl4j.ext.bpel.impl.PartnerLinkTypeImpl;

/**
 * The class represents the populated extension registry.  It extends the
 * populated registry from WSDL4J and registers more WSDL extensions to it.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
class PopulatedExtensionRegistryEx extends PopulatedExtensionRegistry {

    private static final long serialVersionUID = 1L;

    public PopulatedExtensionRegistryEx() {
        super();
        
        BPELExtSerializer bpelExtSerializer = new BPELExtSerializer();
        BPELExtDeserializer bpelExtDeserializer = new BPELExtDeserializer();

        //Extensibility element: property
        registerSerializer(Definition.class,
                           BPELExtConstants.PROPERTY_ELEM,
                           bpelExtSerializer);
        registerDeserializer(Definition.class,
                             BPELExtConstants.PROPERTY_ELEM,
                             bpelExtDeserializer);
        mapExtensionTypes(Definition.class,
                          BPELExtConstants.PROPERTY_ELEM,
                          MessagePropertyImpl.class);

        //Extensibility element: propertyAlias
        registerSerializer(Definition.class,
                           BPELExtConstants.PROPERTY_ALIAS_ELEM,
                           bpelExtSerializer);
        registerDeserializer(Definition.class,
                             BPELExtConstants.PROPERTY_ALIAS_ELEM,
                             bpelExtDeserializer);
        mapExtensionTypes(Definition.class,
                          BPELExtConstants.PROPERTY_ALIAS_ELEM,
                          MessagePropertyAliasImpl.class);

        //Extensibility element: partnerLinkType
        registerSerializer(Definition.class,
                           BPELExtConstants.PARTNER_LINK_TYPE_ELEM,
                           bpelExtSerializer);
        registerDeserializer(Definition.class,
                             BPELExtConstants.PARTNER_LINK_TYPE_ELEM,
                             bpelExtDeserializer);
        mapExtensionTypes(Definition.class,
                          BPELExtConstants.PARTNER_LINK_TYPE_ELEM,
                          PartnerLinkTypeImpl.class);
    }
}
