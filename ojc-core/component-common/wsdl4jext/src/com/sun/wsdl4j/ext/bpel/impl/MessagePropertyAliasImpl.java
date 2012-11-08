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
 * @(#)MessagePropertyAliasImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.NamespaceDeclarations;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias.Query;

/**
 * Implementation of the <code>MessagePropertyAlias</code> interface.
 * 
 * @author Jun Xu
 * @version $Revision: 1.7 $
 */
public class MessagePropertyAliasImpl extends BPELExtElementImpl
        implements MessagePropertyAlias {

    private static final long serialVersionUID = 1L;
    
    protected final Definition _wsdlDef;
    
    protected QName _name;
    protected Message _messageType;
    protected QName _elementName;
    protected QName _typeName;
    protected Part _part;
    protected String _partName;
    protected Query _query;
    protected QName _messageTypeName;
    protected NamespaceDeclarations _namespaceDeclarations;
    protected String _nmPropName;
    
    public MessagePropertyAliasImpl(Definition wsdlDef) {
        _wsdlDef = wsdlDef;
    }
    
    ////////////////////////////////////////////
    //Methods for interface MessagePropertyAlias
    ////////////////////////////////////////////
    
    public Message getMessageType() {
        return _messageType;
    }

    public QName getName() {
        return _name;
    }

    public Part getPart() {
        return _part;
    }

    public String getPartName() {
        if (_part != null) {
            return _part.getName();
        }
        return _partName;
    }

    public Query getQuery() {
        return _query;
    }

    public QName getElementName() {
        return _elementName;
    }

    public QName getTypeName() {
        return _typeName;
    }

    public NamespaceDeclarations getNamespaceDeclarations() {
        return _namespaceDeclarations;
    }
    
    ///////
    //Other
    ///////
    
    public void setMessageType(Message messageType) {
        _messageType = messageType;
    }
    
    public void setElementName(QName elementName) {
        _elementName = elementName;
    }
    
    public void setTypeName(QName typeName) {
        _typeName = typeName;
    }
    
    public void setName(QName name) {
        _name = name;
    }
    
    public void setPart(Part part) {
        _part = part;
    }
    
    public void setPartName(String partName) {
        _partName = partName;
    }
    
    public void setQuery(Query query) {
        _query = query;
    }
    
    public QName getMessageTypeName() {
        return _messageTypeName;
    }
    
    public void setMessageTypeName(QName messageTypeName) {
        _messageTypeName = messageTypeName;
    }
    
    public void setNamespaceDeclarations(NamespaceDeclarations declarations) {
        _namespaceDeclarations = declarations;
    }
    
    public void setNMProperty(String nmPropName) {
        _nmPropName = nmPropName;
    }
    
    /** @see com.sun.wsdl4j.ext.bpel.MessagePropertyAlias#getNMProperty()
     */
    public String getNMProperty() {
        return _nmPropName;
    }

    public static class QueryImpl implements Query {
        
        protected String _lang;
        protected String _queryString;
        protected NamespaceDeclarations _namespaceDeclarations;

        public String getLanguage() {
            return _lang;
        }

        public String getQueryString() {
            return _queryString;
        }
        
        public NamespaceDeclarations getNamespaceDeclarations() {
            return _namespaceDeclarations;
        }
        
        public void setLanguage(String lang) {
            _lang = lang;
        }
        
        public void setQueryString(String queryString) {
            _queryString = queryString;
        }
        
        public void setNamespaceDeclarations(
                NamespaceDeclarations declarations) {
            _namespaceDeclarations = declarations;
        }
    }
}
