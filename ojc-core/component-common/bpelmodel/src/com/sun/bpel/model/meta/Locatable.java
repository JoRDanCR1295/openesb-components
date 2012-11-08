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
 * @(#)Locatable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta;

import org.xml.sax.Locator;


/**
 * Locatable Interface
 *
 * @author Sun Microsystems
 * @version 
 *
 * @since 5.0
 */
public interface Locatable {
    /**
     * gets source location
     *
     * @return Locator source location
     */
    public Locator getSourceLocation();

    /**
     * sets source location
     *
     * @param locator source location
     */
    public void setSourceLocation(Locator locator);

    /**
     * sets line number
     *
     * @param lineNumber line number
     */
    public void setLineNumber(int lineNumber);

    /**
     * sets column number
     *
     * @param columnNumber column number
     */
    public void setColumnNumber(int columnNumber);

    /**
     * sets system ID
     *
     * @param systemId system ID
     */
    public void setSystemId(String systemId);

    /**
     * sets public ID
     *
     * @param publicId public ID
     */
    public void setPublicId(String publicId);

    /**
     * gets line number
     *
     * @return int line number
     */
    public int getLineNumber();

    /**
     * gets column number
     *
     * @return int column number
     */
    public int getColumnNumber();

    /**
     * gets system ID
     *
     * @return String system ID
     */
    public String getSystemId();

    /**
     * gets public ID
     *
     * @return String public ID
     */
    public String getPublicId();
}
