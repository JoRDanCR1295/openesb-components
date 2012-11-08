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
 * @(#)WSDLDefinitionImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc.wsdlmodel.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import javax.wsdl.Definition;
import javax.wsdl.Types;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.sun.jbi.ldapbc.wsdlmodel.WSDLDefinition;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class WSDLDefinitionImpl implements WSDLDefinition {
    private Definition mDefinition;
    private List mAllSchemas;
    private List mInlineSchemas;
    private List mImportedSchemas;

    public WSDLDefinitionImpl(final Definition definition) {
        mDefinition = definition;
        initialize();
    }

    public Definition getDefinition() {
        return mDefinition;
    }

    public Collection getAllSchemas() {
        return mAllSchemas;
    }

    public Collection getInlineSchemas() {
        return mInlineSchemas;
    }

    public Collection getImportedSchemas() {
        return mImportedSchemas;
    }

    private void initialize() {
        mInlineSchemas = loadInlineSchemas();
        mInlineSchemas = loadImportSchemas();
    }

    private List loadInlineSchemas() {
        final List inlineSchemas = new ArrayList();

        final Types types = mDefinition.getTypes();

        if (types != null) {
            final List extElements = types.getExtensibilityElements();

            if (extElements != null) {
                final Iterator it = extElements.iterator();

                while (it.hasNext()) {
                    final ExtensibilityElement exElement = (ExtensibilityElement) it.next();
                    final QName schemaQName = exElement.getElementType();
                    final String ns = schemaQName.getNamespaceURI();
                    final String localName = schemaQName.getLocalPart();

                    if ((ns != null) && ns.equals(WSDLDefinition.NS_SCHEMA) &&
                            localName.equals("schema")) {
                    }
                }
            }
        }

        return inlineSchemas;
    }

    private List loadImportSchemas() {
        //TODO: implement this
        return null;
    }
}
