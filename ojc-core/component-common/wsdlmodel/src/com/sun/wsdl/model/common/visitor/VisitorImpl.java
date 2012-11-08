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
 * @(#)VisitorImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.visitor;

/**
 * Describes the abstract implementation of any XML document visitor.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class VisitorImpl implements Visitor {
    
    /** Holds the visitor service that provided this visitor. */
    private VisitorService visitorService;
    
    /** Holds the visitor support for this visitor. */
    private VisitorSupport visitorSupport;
    
    /** Creates a new instance of VisitorImpl */
    public VisitorImpl() {
    }
    
    /** Resets the visitor for reuse.
     * @return  <code>true</code> if the visitor can reset successfully;
     *          <code>false</code> if it cannot, in which case the visitor
     *          will be reinstantiated.
     */
    public boolean reset() {
        return true;
    }
    
    /** Gets the service that provided this visitor as well as any other
     * needed to traverse other XML documents imported.
     *
     * @return  The visitor service.
     */
    public VisitorService getVisitorService() {
        return visitorService;
    }
    
    /** Sets the service that provided this visitor as well as any other
     * needed to traverse other XML documents imported.
     *
     * @param   s   The visitor service.
     */
    public void setVisitorService(VisitorService s) {
        visitorService = s;
    }
    
    /** Gets the visitor support.
     * @return  Visitor support.
     */
    public VisitorSupport getVisitorSupport() {
        return visitorSupport;
    }
    
    /** Sets the visitor support.
     * @param   s   Visitor support.
     */
    public void setVisitorSupport(VisitorSupport s) {
        visitorSupport = s;
    }
}
