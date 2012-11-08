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
 * @(#)RForEachImplSBYN.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import org.xml.sax.Locator;

import com.sun.bpel.model.extensions.impl.ForEachImpl;
import com.sun.bpel.model.meta.Locatable;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;
import com.sun.bpel.xml.common.model.XMLDocument;


/**
 * Runtime ForEach implementation
 *
 * @author Sun Microsystems
 */
public class RForEachImplSBYN extends ForEachImpl implements RVariableElement, Locatable {
    private int mColumnNumber;
    private int mLineNumber;
    private String mPublicId;
    private Locator mSourceLocation;
    private String mSystemId;

    /**
     * Creates a new instance of RForEachImpl
     *
     * @param bpeldoc runtime BPEL document
     */
    public RForEachImplSBYN(RBPELDocumentImpl bpeldoc) {
        super((XMLDocument) bpeldoc);
    }

    /**
     * gets Variable
     *
     * @return RVariable variable
     */
    public RVariable getRVariable() {
        return (RVariable) super.getBPELVariable();
    }

    /**
     * sets variable
     *
     * @param var variable
     */
    public void setRVariable(RVariable var) {
        // TODO Auto-generated method stub
    }

    /**
     * gets column number
     *
     * @return int column number
     */
    public int getColumnNumber() {
        return mColumnNumber;
    }

    /**
     * gets line number
     *
     * @return int line number
     */
    public int getLineNumber() {
        return mLineNumber;
    }

    /**
     * gets public ID
     *
     * @return String public ID
     */
    public String getPublicId() {
        return mPublicId;
    }

    /**
     * gets source location
     *
     * @return Locator source location
     */
    public Locator getSourceLocation() {
        return mSourceLocation;
    }

    /**
     * gets system ID
     *
     * @return String system ID
     */
    public String getSystemId() {
        return mSystemId;
    }

    /**
     * sets column number
     *
     * @param columnNumber column number
     */
    public void setColumnNumber(int columnNumber) {
        mColumnNumber = columnNumber;
    }

    /**
     * sets line number
     *
     * @param lineNumber line number
     */
    public void setLineNumber(int lineNumber) {
        mLineNumber = lineNumber;
    }

    /**
     * sets public ID
     *
     * @param publicId public ID
     */
    public void setPublicId(String publicId) {
        mPublicId = publicId;
    }

    /**
     * sets source location
     *
     * @param locator source location
     */
    public void setSourceLocation(org.xml.sax.Locator locator) {
        mSourceLocation = locator;
    }

    /**
     * sets system ID
     *
     * @param systemId system ID
     */
    public void setSystemId(String systemId) {
        mSystemId = systemId;
    }
}
