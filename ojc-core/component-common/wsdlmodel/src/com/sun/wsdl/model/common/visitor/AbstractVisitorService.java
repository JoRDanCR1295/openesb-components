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
 * @(#)AbstractVisitorService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.visitor;

/**
 * A base abstract implementation of a visitor service.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class AbstractVisitorService implements VisitorService {
    
    /** Holds an array of all visitor implementation/constructors. */
    private Class[] implClasses;
    
    /** Holds an array of all visitors. */
    private Visitor[] visitors;
    
    /** Creates a new instance of AbstractVisitorService.
     * @param   implClasses     Visitor implementation classes belonging
     *                          to this service.
     */
    public AbstractVisitorService(Class[] implClasses) {
        if (implClasses != null) {
            this.implClasses = implClasses;
            visitors = new Visitor[implClasses.length];
            for (int i = 0; i < visitors.length; i++) {
                try {
                    visitors[i] = (Visitor) implClasses[i].newInstance();
                    visitors[i].setVisitorService(this);
                } catch (Throwable trw) {
                    throw new XMLParseVisitorException(
                        "Cannot instantiate AbstractVisitorService(Class[])", trw);
                }
            }
        }
    }
    
    /** Fetches a visitor.
     * @param   c   Class of visitor (must have a empty constructor).
     * @param   s   Visitor support to be passed in; <code>null</code> if none.
     * @return  A visitor.
     */
    public Visitor fetch(Class c, VisitorSupport s) {
        Visitor visitor = null;
        if ((c != null) && (implClasses != null)) {
            for (int i = 0; i < implClasses.length; i++) {
                if (c.isInstance(visitors[i])) {
                    visitor = visitors[i];
                    visitor.setVisitorSupport(s);
                    break;
                }
            }
        }
        return visitor;
    }
}
