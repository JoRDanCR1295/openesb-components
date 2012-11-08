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
 * @(#)RStartElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta;

/**
 * runtime start element
 *
 * @author Sun Microsystems
 */
public interface RStartElement extends RVariableElement, RMessagingElement {
    /**
     * gets create instance flag
     *
     * @return boolean create instance flag
     */
    boolean getRCreateInstance();

    /**
     * gets mesage exchange ID
     *
     * @return String message exchange ID
     */
    String getMessageExchange();

    // TODO add reply matching support
    //    Reply getMatchingReply();
    /**
     * get correlation definition wrapper
     *
     * @return correlation definition wrapper
     */
    RStartElementCorrelationDefnWrapper getCorrelationDefnWrapper();

    /**
     * DOCUMENT ME!
     *
     * @param corrDefn DOCUMENT ME!
     */
    void setCorrelationDefnWrapper(RStartElementCorrelationDefnWrapper corrDefn);

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    int getStartType();

    /**
     * DOCUMENT ME!
     *
     * @param startType DOCUMENT ME!
     */
    void setStartType(int startType);
}
