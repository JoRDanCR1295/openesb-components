/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport.common;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 *
 * @author chikkala
 */
public interface JbiDescriptor {
    
    public static final String BINDING_COMPONENT_TYPE = "binding-component";
    public static final String SERVICE_ENGINE_TYPE = "service-engine";
    public static final String SHARED_LIBRARY_TYPE = "shared-library";
    public static final String SERVICE_ASSEMBLY_TYPE = "service-assembly";
    
    public static final String PARENT_FIRST_CL_DELEGATION = "parent-first";
    public static final String SELF_FIRST_CL_DELEGATION = "self-first";
    
    /**
     * Getter for property DOMDocument.
     * @return Value of property DOMDocument.
     */
    public Document getDOMDocument();
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getName();
    
    /**
     * Setter for property name.
     * @param name New value of property name.
     */
    public void setName(String name);
    
    /**
     * Getter for property description.
     * @return Value of property description.
     */
    public String getDescription();
    
    /**
     * Setter for property description.
     * @param description New value of property description.
     */
    public void setDescription(String description);
    
    public static abstract class AbstractJbiDescriptor implements JbiDescriptor {
        
        private Document mDOMDoc;
        
        private Element mJbiEl;
        
        protected AbstractJbiDescriptor(Document domDocument) {
            this.mDOMDoc = domDocument;
        }
        
        public Document getDOMDocument() {
            return this.mDOMDoc;
        }
        
        protected Element getJbiElement() {
            if ( this.mJbiEl == null ) {
                this.mJbiEl = DOMUtil.UTIL.getElement(this.mDOMDoc, "jbi");
                if ( this.mJbiEl == null ) {
                    this.mJbiEl = this.mDOMDoc.createElement("jbi");
                    this.mDOMDoc.appendChild(this.mJbiEl);
                }
            }
            return this.mJbiEl;
        }
        
    }
    
}
