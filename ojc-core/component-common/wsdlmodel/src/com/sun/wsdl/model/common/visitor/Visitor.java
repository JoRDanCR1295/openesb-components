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
 * @(#)Visitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.visitor;

/**
 * A base interface to describe a generic visitor to a model.  <B>DO
 * NOT implement this interface directly.</B>
 *
 * @author Sun Microsystems
 * @version 
 * @see AutonomousVisitor
 * @see ChildrenParentVisitor
 * @see ParentChildrenParentVisitor
 * @see ParentChildrenVisitor
 */
public interface Visitor {
    
    /** Resets the visitor for reuse.
     * @return  <code>true</code> if the visitor can reset successfully;
     *          <code>false</code> if it cannot, in which case the visitor
     *          will be reinstantiated.
     */
    boolean reset();
    
    /** Gets the service that provided this visitor as well as any other
     * needed to traverse other XML documents imported.
     *
     * @return  The visitor service.
     */
    VisitorService getVisitorService();
    
    /** Sets the service that provided this visitor as well as any other
     * needed to traverse other XML documents imported.
     *
     * @param   s   The visitor service.
     */
    void setVisitorService(VisitorService s);
    
    /** Gets the visitor support.
     * @return  Visitor support.
     */
    VisitorSupport getVisitorSupport();
    
    /** Sets the visitor support.
     * @param   s   Visitor support.
     */
    void setVisitorSupport(VisitorSupport s);
    
}
