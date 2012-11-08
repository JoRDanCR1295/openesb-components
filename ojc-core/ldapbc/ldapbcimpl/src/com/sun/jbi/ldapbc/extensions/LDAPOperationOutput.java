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
 * @(#)LDAPOperationOutput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc.extensions;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.List;
import javax.wsdl.Message;
import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LDAPOperationOutput implements ExtensibilityElement, Serializable {
    public static final String ATTR_RETURN_PART_NAME = "returnPartName";
    public static final String ATTR_ATTRIBUTES = "attributes";
    private static final long serialVersionUID = 1L;
    private QName mFieldElementType = LDAPConstants.QNAME_OPERATION;
    private Boolean mFieldRequired = null;
    private String mReturnPartName;
    private String mAttributes = null;
    private String name = null;
    private Message message = null;
    private List returnAttrs;

    public LDAPOperationOutput() {
    }
    
    public void addReturnAttr(String attr){
        if(returnAttrs==null){
            returnAttrs=new ArrayList();
        }
        returnAttrs.add(attr);
    }
    
    public String toString(){
        String ret="";
        if(returnAttrs!=null){
            for(int i=0;i<returnAttrs.size();i++)
            ret+=returnAttrs.get(i);
        }
        return ret;
    }
    
    public List getReturnAttrs(){
        return this.returnAttrs;
    }

    /**
     * @return mFieldElementType
     */
    public QName getElementType() {
        return mFieldElementType;
    }

    /**
     * @return mFieldRequired
     */
    public Boolean getRequired() {
        return mFieldRequired;
    }

    public void setElementType(final QName elementType) {
        mFieldElementType = elementType;
    }
    
    public void setAttributes(String a) {
        mAttributes = a;
    }
    

    public void setRequired(final Boolean required) {
        mFieldRequired = required;
    }

    /**
     *
     * @return mReturnPartName
     */
    public String getReturnPartName() {
        return mReturnPartName;
    }

    public void setReturnPartName(final String returnPartName) {
        mReturnPartName = returnPartName;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setMessage(final Message mesg) {
        message = mesg;
    }

    public Message getMessage() {
        return message;
    }
    
    public String getAttributes() {
        return mAttributes;
    }
}
